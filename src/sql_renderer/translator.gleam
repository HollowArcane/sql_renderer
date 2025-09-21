import toolkit_util/lists
import gleam/pair
import gleam/dict.{type Dict}
import gleam/float
import gleam/int
import gleam/string
import given
import gleam/option.{Some, None}
import gleam/result
import sql_renderer/renderer.{SQLArgument, NativeType, Varchar, Integer, type SQLVariable}
import gleam/list
import glance.{Definition, Function, type FunctionParameter, FunctionParameter, Named, Discarded, type Type, type Statement, type Expression}

pub type TranslationError
{
    TranslationError(
        span: glance.Span,
        error: String,
    )
}

pub fn translate(module: glance.Module)
    -> Result(renderer.SQLModule, TranslationError)
{
    // ideally:
    // custom_types -> table
    // function -> function / procedure / view / tiggers
    // types_aliases / constant -> hints to replace certain values
    let glance.Module(imports:_, custom_types:_, type_aliases:_, constants:_, functions:) = module
    
    let constant_map = dict.new()
    // register all functions
    let constant_map = {
        use constant_map, function <- list.fold(functions, from: constant_map)
        case extract_external_attribute(function)
        {
            Error(_) -> constant_map
            Ok(var_name) -> constant_map |> dict.insert(
                function.definition.name,
                renderer.SQLVariable(var_name, NativeType(Varchar))
            )
        }
    }
    echo constant_map

    use elements <- result.try({
        use elements, function <- list.try_fold(functions, from: [])
        use function <- result.try(
            translate_function(function, constant_map)
        )
         
        case function
        {
            None -> elements
            Some(function) -> [renderer.Function( function ), ..elements]
        } |> Ok
    })
    Ok(elements)
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

fn translate_argument(argument: FunctionParameter, span)
{
    let FunctionParameter(label:_, name:, type_:) = argument
    use type_ <- given.some(type_, else_return: fn() {Error(TranslationError(
        span:, error: "Function arguments need to be typed" 
    ))})
    use type_ <- result.try(
        translate_type(type_)
    )
    case name
    {
        Named(name) -> Some(SQLArgument(name: Some(name), type_:))
        Discarded(_) -> None
    } |> Ok
}

fn translate_body_loop(body: List(Statement), constant_map: Dict(String, SQLVariable), translated: List(renderer.SQLStatement))
{
    case body
    {
        [] -> Ok([])

        [glance.Expression(expression)] -> {
            use #(expression, constant_map) <- result.map(expression |> translate_expression(constant_map))
            [renderer.Return(expression), ..translated]
        }

        [ glance.Assignment(value:, ..) ] -> {
            use #(expression, constant_map) <- result.map(value |> translate_expression(constant_map))
            [renderer.Return(expression), ..translated]
        }

        [_] -> {
            Error(TranslationError(span: glance.Span(start:0, end: 0), error:"Last line should be a returnable statement"))
        }

        [statement, ..rest] -> {
            use #(statement, constant_map) <- result.try(case statement
            {
                glance.Use(location:, patterns:_, function:_) -> 
                    Error(TranslationError(span: location, error:"use syntax is not supported"))

                glance.Assignment(location:_, kind:_, pattern:_, annotation:_, value:_) -> todo as "need to implement an assignment system, make annotation mandatory for now"
                
                glance.Assert(location:, expression:_, message:_) ->
                    Error(TranslationError(span: location, error:"assert syntax is not supported"))

                glance.Expression(expression) ->  expression |> translate_expression(constant_map) |> result.map(pair.map_first(_, renderer.Expression))
            })
            translate_body_loop(rest, constant_map, [statement, ..translated])
        }
    }
}

fn translate_body(body: List(Statement), constant_map)
{ translate_body_loop(body, constant_map, []) }

fn translate_expression(
    expression: Expression,
    constant_map: Dict(String, SQLVariable)
) -> Result(#(renderer.SQLExpression, Dict(String, SQLVariable)), TranslationError)
{
    case expression
    {
        glance.Int(location:_, value:) -> {
            let assert Ok(value) = int.parse(value)
            Ok(#(renderer.NumericIntLiteral(value), constant_map))
        }

        glance.Float(location:_, value:) -> {
            let assert Ok(value) = float.parse(value)
            Ok(#(renderer.NumericFloatLiteral(value), constant_map))
        }

        glance.String(location:_, value:) ->
            Ok(#(renderer.VarcharLiteral(value), constant_map))

        glance.Variable(location:_, name:) -> case constant_map |> dict.get(name)
        {
            Ok(var) -> Ok(#(renderer.VariableCall(var), constant_map))
            // error means it's not a global constant, rather a local variable
            Error(_) -> Ok(#(renderer.VariableCall(renderer.SQLVariable(
                name:, type_: NativeType(Varchar) // maybe this type is not necessary
            )), constant_map))
        }

        glance.NegateInt(location:, value:) -> case value
        {
            glance.Int(location:_, value:) -> {
                let assert Ok(value) = int.parse(value)
                Ok(#(renderer.NumericIntLiteral(-value), constant_map))
            }

            glance.Float(location:_, value:) -> {
                let assert Ok(value) = float.parse(value)
                Ok(#(renderer.NumericFloatLiteral(-1. *. value), constant_map))
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

        glance.Call(location:, function:, arguments:) -> {
            use #(function, constant_map) <- result.try(
                function |> translate_expression(constant_map)
            )
            use #(constant_map, arguments) <- result.try({
                use constant_map, arg <- lists.try_map_fold(arguments, constant_map)
                case arg
                {
                    glance.LabelledField(label:_, item:) -> item |> translate_expression(constant_map) |> result.map(pair.swap)

                    glance.ShorthandField(label:) -> label |> glance.Variable(location:) |> translate_expression(constant_map) |> result.map(pair.swap)

                    glance.UnlabelledField(item:) -> item |> translate_expression(constant_map) |> result.map(pair.swap)
                }
                
            })

            Ok(#(renderer.FunctionCall(function:, arguments:), constant_map))
        }

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

        glance.BinaryOperator(location:, name:_, left:_, right:_) ->
            Error(TranslationError(span: location,
                error: "BitArrays are not supported"
            ))

        glance.Echo(location:, expression:_) -> 
            Error(TranslationError(span: location,
                error: "echo syntax is not supported"
            ))
    }
}

fn translate_function(function: glance.Definition(glance.Function), constant_map: Dict(String, SQLVariable))
{
    let Definition(attributes:, definition:) = function
    let Function(location:, name:, publicity:_, parameters:, return:, body:) = definition
    
    use return <- given.some(return, else_return: fn() {Error(TranslationError(
        span: location, error: "Function return type need to be declared" 
    ))})
    
    use args <- result.try({
        use parameters, parameter <- list.try_fold(parameters, from: [])
        use parameter <- result.try(parameter |> translate_argument(location))
        case parameter
        {
            None -> Ok(parameters)
            Some(parameter) -> Ok( [parameter, ..parameters] )
        }
    })
    let args = list.reverse(args)
    use return_type <- result.try(return |> translate_type)

    use _ <- given.ok(
        extract_external_attribute(function), // don't translate body if it is an external function
        return: fn(_) {Ok(None)}
    )

    use body <- result.try(body |> translate_body(constant_map))
    Some(renderer.SQLFunction(name:, args:, return_type:, body:)) |> Ok
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
