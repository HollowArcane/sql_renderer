import toolkit_util/recursions
import gleam/float
import gleam/int
import gleam/string
import given
import gleam/option.{Some, None}
import gleam/result
import sql_renderer/renderer.{SQLArgument, NativeType, Varchar, Integer}
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
    
    use elements <- result.try({
        use elements, function <- list.try_fold(functions, from: [])
        use function <- result.try(
            translate_function(function)
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

fn translate_body_loop(body: List(Statement), translated: List(renderer.SQLStatement))
{
    case body
    {
        [] -> Ok([])

        [glance.Expression(expression)] -> {
            use expression <- result.map(expression |> translate_expression)
            [renderer.Return(expression), ..translated]
        }

        [ glance.Assignment(value:, ..) ] -> {
            use expression <- result.map(value |> translate_expression)
            [renderer.Return(expression), ..translated]
        }

        [_] -> {
            Error(TranslationError(span: glance.Span(start:0, end: 0), error:"Last line should be a returnable statement"))
        }

        [statement, ..rest] -> {
            use statement <- result.try(case statement
            {
                glance.Use(location:, patterns:_, function:_) -> 
                    Error(TranslationError(span: location, error:"use syntax is not supported"))

                glance.Assignment(location:_, kind:_, pattern:_, annotation:_, value:_) -> todo as "need to implement an assignment system, make annotation mandatory for now"
                
                glance.Assert(location:, expression:_, message:_) ->
                    Error(TranslationError(span: location, error:"assert syntax is not supported"))

                glance.Expression(expression) ->  expression |> translate_expression |> result.map(renderer.Expression)
            })
            translate_body_loop(rest, [statement, ..translated])
        }
    }
}

fn translate_body(body: List(Statement))
{ translate_body_loop(body, []) }

fn translate_expression(expression: Expression)
{
    case expression
    {
        glance.Int(location:_, value:) -> {
            let assert Ok(value) = int.parse(value)
            Ok(renderer.NumericIntLiteral(value))
        }

        glance.Float(location:_, value:) -> {
            let assert Ok(value) = float.parse(value)
            Ok(renderer.NumericFloatLiteral(value))
        }

        glance.String(location:_, value:) -> Ok(renderer.VarcharLiteral(value))

        glance.Variable(location:_, name:) -> Ok(renderer.VariableCall(renderer.SQLVariable(name:, type_: renderer.NativeType( renderer.Varchar )))) // REGISTRY SHOULD BE IMPLEMENTED HERE

        glance.NegateInt(location:, value:) -> case value
        {
            glance.Int(location:_, value:) -> {
                let assert Ok(value) = int.parse(value)
                Ok(renderer.NumericIntLiteral(-value))
            }

            glance.Float(location:_, value:) -> {
                let assert Ok(value) = float.parse(value)
                Ok(renderer.NumericFloatLiteral(-1. *. value))
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
            use function <- result.try(
                function |> translate_expression
            )
            use arguments <- result.try({
                use arg <- list.try_map(arguments)
                case arg
                {
                    glance.LabelledField(label:_, item:) -> translate_expression(item)
                    glance.ShorthandField(label:) -> translate_expression(label |> glance.Variable(location:))
                    glance.UnlabelledField(item:) -> translate_expression(item)
                }
                
            })

            Ok( renderer.FunctionCall(function:, arguments:) )
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

fn translate_function(function: glance.Definition(glance.Function))
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

    use <- given.that(case attributes
    {
        [glance.Attribute(name: "external", arguments: [
            glance.Variable(..), glance.String(value: "sql", ..), glance.String(..)
        ])] -> True
        _ -> False
    }, return: fn() {Ok(None)})

    use body <- result.try(body |> translate_body)
    Some(renderer.SQLFunction(name:, args:, return_type:, body:)) |> Ok
}
