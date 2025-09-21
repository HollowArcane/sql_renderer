import gleam/string
import given
import gleam/option.{Some, None}
import gleam/result
import sql_renderer/renderer.{SQLArgument, NativeType, Varchar, Integer}
import gleam/list
import glance.{Definition, Function, type FunctionParameter, FunctionParameter, Named, Discarded, type Type, type Statement}

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

fn translate_body(body: List(Statement))
{
    use statements, statement <- list.try_fold(body, [])
    case statement
    {
        glance.Use(location:, patterns:_, function:_) -> 
            Error(TranslationError(span: location, error:"use syntax is not supported"))

        glance.Assignment(location:, kind:, pattern:, annotation:, value:) -> todo
        
        glance.Assert(location:, expression:, message:) ->
            Error(TranslationError(span: location, error:"assert syntax is not supported"))

        glance.Expression(expression) ->  todo
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
    Some(renderer.SQLFunction(name:, args:, return_type:, body: [])) |> Ok
}
