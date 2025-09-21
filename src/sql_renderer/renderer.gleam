import gleam/option.{type Option, None, Some}
import gleam/list
import gleam/string_tree.{type StringTree}

pub type SQLModule = List(SQLElement)
pub type SQLElement
{
    Function(SQLFunction)
}

pub fn render(module: SQLModule)
{ string_tree.to_string(module |> render_module) }

fn render_module(module: SQLModule)
{
    use tree, element <- list.fold(module, from: string_tree.new())
    case element
    {
        Function(function) -> tree
            |> string_tree.append_tree(
                function
                    |> render_function
                    |> string_tree.join("\n")
            )
            |> string_tree.append("\n")
    }
}

pub type SQLNativeType
{
    Varchar
    Integer
    // anything else
}

pub type SQLType
{
    NativeType(SQLNativeType)
}

pub type SQLArgument
{
    SQLArgument(
        name: Option(String),
        type_: SQLType,
    )
}

pub type SQLStatement

pub type SQLFunction
{
    SQLFunction (
        name: String,
        args: List(SQLArgument),
        return_type: SQLType,
        body: List(SQLStatement),
    )
}

fn render_type(type_: SQLType)
{
    case type_
    {
        NativeType(type_) -> case type_
        {
            Integer -> string_tree.from_string("INTEGER")
            Varchar -> string_tree.from_string("VARCHAR")
        }
    }
}
    

fn render_argument(argument: SQLArgument)
{
    let SQLArgument(name:, type_:) = argument
    let buffer = case name
        {
            None -> string_tree.new()
            Some(name) -> string_tree.from_string(name)
                |> string_tree.append(" ")

        }
    buffer |> string_tree.append_tree(render_type(type_))
}


fn render_function_signature(name, args, return_type)
{
    string_tree.from_string("CREATE OR REPLACE FUNCTION ")
        |> string_tree.append(name)
        |> string_tree.append(" (")
        |> string_tree.append_tree(
            args
                |> list.map(render_argument)
                |> string_tree.join(", ")
        )
        |> string_tree.append(") RETURNS ")
        |> string_tree.append_tree(render_type(return_type))
}

fn render_function(function: SQLFunction)
{
    let SQLFunction(name:, args:, return_type:, body:_) = function
    [
        render_function_signature(name, args, return_type)
            |> string_tree.append(" AS $$"),
        string_tree.from_string("BEGIN"),
        // insert body here 
        string_tree.from_string("END;"),
        string_tree.from_string("$$ LANGUAGE plpgsql"),
    ]
}
