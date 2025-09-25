import sql_renderer/registry.{type Registry}
import toolkit_util/lists
import gleam/pair
import gleam/float
import gleam/int
import gleam/string
import given
import gleam/option.{Some, None}
import gleam/result
import sql_renderer/renderer.{SQLArgument, NativeType, Varchar, Integer, type SQLExpression, SQLVariable, Real, Addition, Multiplication, Substraction}
import gleam/list
import glance.{Definition, Function, type FunctionParameter, FunctionParameter, Named, Discarded, type Type, type Statement, type Expression, Assignment}

pub type TranslationError
{
    TranslationError(
        span: glance.Span,
        error: String,
    )
}

fn scan_constants(
    registry: Registry,
    constants: List(glance.Definition(glance.Constant)),
)
{
    // register all functions
    use registry, constant <- list.try_fold(constants, from: registry)
    let glance.Constant(name:, value:, ..) = constant.definition

    use #(expression, registry) <- result.map(
        registry |> translate_expression(value)
    )
    registry |> registry.write_constant(name, expression)
}


fn scan_external_functions(
    registry: Registry,
    functions: List( glance.Definition(glance.Function) )
)
{
    // register all functions
    use registry, function <- list.fold(functions, from: registry)
    case extract_external_attribute(function)
    {
        Error(_) -> registry
        Ok(var_name) -> registry |> registry.write_constant(
            function.definition.name,
            renderer.VariableCall(var_name)
        )
    }
}

fn translate_and_collect_functions(registry: Registry, functions)
{
    use elements, function <- list.try_fold(functions, from: [])
    use function <- result.try(registry |> 
        type_check_and_translate_function_if_not_external(function)
    )
     
    case function
    {
        None -> elements
        Some(function) -> [renderer.Function( function ), ..elements]
    } |> Ok
}

pub fn translate(module: glance.Module)
    -> Result(renderer.SQLModule, TranslationError)
{
    // ideally:
    // custom_types -> table
    // function -> function / procedure / view / tiggers
    // types_aliases / constant -> hints to replace certain values
    let glance.Module(imports:_, custom_types:_, type_aliases:_, constants:, functions:) = module
    
    use registry <- result.try({
        registry.new()
            |> scan_external_functions(functions)
            |> scan_constants(constants)
    })

    echo registry

    registry |> translate_and_collect_functions(functions)
}

fn translate_type(type_: Type)
{
    case type_
    {
        glance.NamedType(location:, name:, module:, parameters:_) ->
            // should be something like:
            // case imports |> get_definition(name, module)
            // {
            //     "sql_renderer/types.Varchar" -> ..
            case module, name
            {
                Some("types"), "Varchar" ->
                    Ok(NativeType(Varchar))
                    
                Some("types"), "Integer" ->
                    Ok(NativeType(Integer))

                Some("types"), "Real" ->
                    Ok(NativeType(Real))


                _, _ -> 
                    Error(TranslationError(span: location, error:"Unknown type: " <> string.inspect(type_) <> ". Please use types exposed in sql_renderer.types module"))
            }

        glance.TupleType(location:, elements:_) ->
            Error(TranslationError(span: location, error:"Standard gleam types are not supported and that includes tuples. Please use types exposed in sql_renderer.types module"))
        
        glance.FunctionType(location:, parameters:_, return:_) ->
            Error(TranslationError(span: location, error:"Callbacks are not supported. Please use types exposed in sql_renderer.types module"))
        
        glance.VariableType(location:, name:_) ->
            Error(TranslationError(span: location, error:"Generic types are not supported. Please use types exposed in sql_renderer.types module"))

        glance.HoleType(location:, name:_) ->
            Error(TranslationError(span: location, error:"Hole types are not supported. Please use types exposed in sql_renderer.types module"))
    }


}

fn translate_assignment(registry, name, type_, value, location)
{
    use #(expression, registry) <- result.try(
        registry |> translate_expression(value)
    )
    use type_ <- given.some(type_, else_return: fn() {Error(TranslationError(
        span: location, error: "Variable type need to be declared" 
    ))})
    use type_ <- result.try(type_ |> translate_type)
    let #(registry, variable) = registry
        |> registry.write_variable(name, type_)
    #(renderer.Assignment(variable, expression), registry) |> Ok
    
}

fn translate_body_loop(
    registry: Registry,
    body: List(Statement),
    translated: List(renderer.SQLStatement)
) -> Result(#(List(renderer.SQLStatement), Registry), TranslationError)
{
    case body
    {
        [] -> Ok(#([], registry))

        [glance.Expression(expression)] -> {
            use #(expression, registry) <- result.map(registry |> translate_expression(expression))
            #([renderer.Return(expression), ..translated], registry)
        }

        [ Assignment(value:, ..) ] -> {
            use #(expression, registry) <- result.map(registry |> translate_expression(value))
            #([renderer.Return(expression), ..translated], registry)
        }

        [_] -> {
            Error(TranslationError(span: glance.Span(start:0, end: 0), error:"Last line should be a returnable statement"))
        }

        [statement, ..rest] -> {
            use #(statement, registry) <- result.try(case statement
            {
                glance.Use(location:, patterns:_, function:_) -> 
                    Error(TranslationError(span: location, error:"use syntax is not supported"))

                Assignment(pattern: glance.PatternVariable(name:, ..), annotation:, value:, location:, ..) -> registry |> translate_assignment(name, annotation, value, location)

                Assignment(location:_, kind:_, pattern:_, annotation:_, value:_) -> todo as "need to implement an assignment system, make annotation mandatory for now"
                
                glance.Assert(location:, expression:_, message:_) ->
                    Error(TranslationError(span: location, error:"assert syntax is not supported"))

                glance.Expression(expression) ->  registry |> translate_expression(expression) |> result.map(pair.map_first(_, renderer.Expression))
            })
            registry |> translate_body_loop(rest, [statement, ..translated])
        }
    }
}

fn translate_body(registry: Registry, body: List(Statement))
{
    use #(body, registry) <- result.map(
        registry |> translate_body_loop(body, [])
    )
    #(list.reverse(body), registry)
}

fn translate_function_call(registry, function, arguments, location)
{
    use #(function, registry) <- result.try(
        registry |> translate_expression(function)
    )
    use #(constant_map, arguments) <- result.try({
        use constant_map, arg <- lists.try_map_fold(arguments, registry)
        case arg
        {
            glance.LabelledField(label:_, item:) -> constant_map |> translate_expression(item) |> result.map(pair.swap)

            glance.ShorthandField(label:) -> constant_map
                |> translate_expression(label
                    |> glance.Variable(location:)
                ) |> result.map(pair.swap)

            glance.UnlabelledField(item:) -> registry |> translate_expression(item) |> result.map(pair.swap)
        }
        
    })

    Ok(#(renderer.FunctionCall(function:, arguments:), constant_map))
}

fn translate_binary_operator(operator: glance.BinaryOperator)
{
    case operator
    {
        glance.AddFloat -> Addition
        glance.AddInt -> Addition
        glance.And -> renderer.And
        glance.Concatenate -> renderer.Concatenate
        glance.DivFloat -> renderer.FloatDivision
        glance.DivInt -> renderer.IntDivision
        glance.Eq -> renderer.Equals
        glance.GtEqFloat -> renderer.GreaterOrEqual
        glance.GtEqInt -> renderer.GreaterOrEqual
        glance.GtFloat -> renderer.GreaterThan
        glance.GtInt -> renderer.GreaterThan
        glance.LtEqFloat -> renderer.LessOrEqual
        glance.LtEqInt -> renderer.LessOrEqual
        glance.LtFloat -> renderer.LessThan
        glance.LtInt -> renderer.LessThan
        glance.MultFloat -> Multiplication
        glance.MultInt -> Multiplication
        glance.NotEq -> renderer.NotEquals
        glance.Or -> renderer.Or
        glance.Pipe -> todo
        glance.RemainderInt -> renderer.Modulo
        glance.SubFloat -> Substraction
        glance.SubInt -> Substraction
    }
}
fn translate_binary_operation(registry, operator, left, right)
{
    use #(expression1, registry) <- result.try(
        registry |> translate_expression(left)
    )
    use #(expression2, registry) <- result.map(
        registry |> translate_expression(right)
    )
    let operator = translate_binary_operator(operator)
    #(renderer.BinaryOperation(operator, expression1, expression2), registry)
}

fn translate_expression(
    registry: Registry,
    expression: Expression,
) -> Result(#(SQLExpression, Registry), TranslationError)
{
    case expression
    {
        glance.Int(location:_, value:) -> {
            let assert Ok(value) = int.parse(value)
            Ok(#(renderer.NumericIntLiteral(value), registry))
        }

        glance.Float(location:_, value:) -> {
            let assert Ok(value) = float.parse(value)
            Ok(#(renderer.NumericFloatLiteral(value), registry))
        }

        glance.String(location:_, value:) ->
            Ok(#(renderer.VarcharLiteral(value), registry))

        glance.Variable(location:_, name:) ->
            case registry |> registry.get_constant(name)
            {
                Ok(expr) -> Ok(#(expr, registry))
                // error means it's not a global constant, rather a local variable
                Error(_) -> Ok(#(renderer.VariableCall(name), registry))
            }

        glance.NegateInt(location:, value:) -> case value
        {
            glance.Int(location:_, value:) -> {
                let assert Ok(value) = int.parse(value)
                Ok(#(renderer.NumericIntLiteral(-value), registry))
            }

            glance.Float(location:_, value:) -> {
                let assert Ok(value) = float.parse(value)
                Ok(#(renderer.NumericFloatLiteral(-1. *. value), registry))
            }

            _ -> Error(TranslationError(span: location,
                error: "Unexpected negative value"
            ))
        }

        glance.NegateBool(location:, value:_) ->
            Error(TranslationError(span: location,
                error: "Bool negation operator is not supported"
            ))

        glance.Block(location:, statements:_) ->
            Error(TranslationError(span: location,
                error: "Blocks are not supported"
            ))

        glance.Panic(location:, message:_) ->
            Error(TranslationError(span: location,
                error: "panic syntax is not supported"
            ))
            
        glance.Todo(location:, message:_) -> 
            Error(TranslationError(span: location,
                error: "todo syntax is not supported"
            ))

        glance.Tuple(location:, elements:_) -> 
            Error(TranslationError(span: location,
                error: "Tuples are not supported"
            ))

        glance.List(location:, elements:_, rest:_) -> 
            Error(TranslationError(span: location,
                error: "Tuples are not supported"
            ))

        glance.Fn(location:, arguments:_, return_annotation:_, body:_) -> 
            Error(TranslationError(span: location,
                error: "Callbacks are not supported"
            ))

        glance.RecordUpdate(location:, module:_, constructor:_, record:_, fields:_) -> 
            Error(TranslationError(span: location,
                error: "Records are not supported"
            ))

        glance.FieldAccess(location:, container:_, label:_) -> 
            Error(TranslationError(span: location,
                error: "Records are not supported"
            ))

        glance.Call(location:, function:, arguments:) -> 
            registry |> translate_function_call(function, arguments, location)

        glance.TupleIndex(location:, tuple:_, index:_) -> 
            Error(TranslationError(span: location,
                error: "Tuples are not supported"
            ))

        glance.FnCapture(location:, label:_, function:_, arguments_before:_, arguments_after:_) -> 
            Error(TranslationError(span: location,
                error: "Callbacks are not supported"
            ))

        glance.BitString(location:, segments:_) -> 
            Error(TranslationError(span: location,
                error: "BitArrays are not supported"
            ))

        glance.Case(location:, subjects:_, clauses:_) ->
            Error(TranslationError(span: location,
                error: "case expressions are not supported"
            ))

        glance.BinaryOperator(name:, left:, right:, ..) ->
            registry |> translate_binary_operation(name, left, right)


        glance.Echo(location:, expression:_) -> 
            Error(TranslationError(span: location,
                error: "echo syntax is not supported"
            ))
    }
}
fn translate_function_return_type(function: glance.Function)
{
    let Function(location:, return:, ..) = function
    use return_type <- given.some(return, else_return: fn() {Error(TranslationError(
        span: location, error: "Function return type need to be declared" 
    ))})
    return_type |> translate_type
}

//fn require_sql_type(type_: option.Option(glance.Type), scope, then)
//{
//        given.some(return, else_return: fn() {Error(TranslationError(
//            span: location, error: scope <> "type need to be declared" 
//        ))}, return: then)
//}

fn translate_parameter_if_not_discarded(argument: FunctionParameter, span)
{
    let FunctionParameter(label:_, name:, type_:) = argument
    use type_ <- given.some(type_, else_return: fn() {Error(TranslationError(
        span:, error: "Function arguments need to be typed" 
    ))})

    use type_ <- result.try(translate_type(type_))
    case name
    {
        Named(name) -> Some(SQLArgument(name: Some(name), type_:))
        Discarded(_) -> None
    } |> Ok
}


fn translate_and_collect_parameters(parameters, location)
{
    {
        use parameters, parameter <- list.try_fold(parameters, from: [])
        use parameter <- result.try(parameter |>
            translate_parameter_if_not_discarded(location)
        )
        case parameter
        {
            None -> Ok(parameters)
            Some(parameter) -> Ok( [parameter, ..parameters] )
        }
    } |> result.map(list.reverse)
    
}

pub fn type_check_and_translate_function_if_not_external(
    registry: Registry,
    definition: glance.Definition(glance.Function),
)
{
    let Definition(attributes:_, definition: function) = definition
    let Function(location:, name:, publicity:_, parameters:, return:_, body:) = function
    
    use return_type <- result.try(function
        |> translate_function_return_type()
    )
    use args <- result.try(parameters
        |> translate_and_collect_parameters(location)
    )

    use _ <- given.ok(
        extract_external_attribute(definition), // don't translate body if it is an external function
        return: fn(_) {Ok(None)}
    )

    use #(body, registry) <- result.try(registry |> translate_body(body))
    let declarations = registry |> registry.compute_variable_declarations()
    Some(renderer.SQLFunction(name:, args:, return_type:, body:, declarations:)) |> Ok
}

pub fn extract_external_attribute(function: glance.Definition(any))
{
    let Definition(attributes:, definition:_) = function
    use attribute <- list.find_map(attributes)
    case attribute
    {
        glance.Attribute(name: "external", arguments: [
            glance.Variable(..),
            glance.String(value: "sql", ..),
            glance.String(value: external_name, ..)
        ]) -> Ok(external_name)
        _ -> Error(Nil)
    }
}
