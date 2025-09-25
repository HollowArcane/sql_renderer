import toolkit_util/recursions
import gleam/int
import gleam/float
import gleam/option.{type Option, None, Some}
import gleam/list
import gleam/string_tree.{type StringTree}

pub type SQLModule = List(SQLElement)
pub type SQLElement
{
    Function(SQLFunction)
}

pub fn render(module: SQLModule)
{ string_tree.to_string(module |> render_module |> list.reverse |> string_tree.join("\n")) }

fn render_module(module: SQLModule)
{
    use buffer, element <- list.fold(module, from: [])
    case element
    {
        Function(function) -> function |> render_function(buffer)
    }
}

pub type SQLNativeType
{
    Varchar
    Integer
    Real
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

pub type SQLVariable
{
    SQLVariable(
        name: String,
        type_: SQLType,
    )
}

pub type SQLOperator
{
    Addition
    Substraction
    Multiplication
    IntDivision
    FloatDivision
    And
    Or
    Equals
    NotEquals
    Concatenate
    Modulo
    GreaterThan
    GreaterOrEqual
    LessThan
    LessOrEqual
}

pub type SQLExpression
{
    VariableCall(variable: String)
    NumericFloatLiteral(value: Float)
    NumericIntLiteral(value: Int)
    BinaryOperation(operator: SQLOperator, e1: SQLExpression, e2: SQLExpression)
    VarcharLiteral(value: String)
    FunctionCall(function: SQLExpression, arguments: List(SQLExpression))
}

pub type SQLStatement
{
    Assignment(variable: SQLVariable, expression: SQLExpression)
    Expression(expression: SQLExpression)
    Return(expression: SQLExpression)
}

pub type SQLFunction
{
    SQLFunction (
        name: String,
        args: List(SQLArgument),
        declarations: List(SQLVariable),
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
            Real -> string_tree.from_string("REAL")
        }
    }
}

fn render_variable(variable: SQLVariable)
{
    let SQLVariable(name:, type_:) = variable
    let buffer =  string_tree.from_string(name)
    buffer
        |> string_tree.append(" ")
        |> string_tree.append_tree(render_type(type_))
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

fn render_function_call(function function, arguments arguments, buffer buffer, prefix prefix, suffix suffix)
{
    let #(prefix, buffer) = function |> render_expression(
            buffer:,
            prefix:,
            suffix: string_tree.from_string("("),
        )
    {
        use #(buffer, prefix, arguments) <-
            recursions.start(#(buffer, prefix, arguments))

        case arguments
        {
            [] -> recursions.End( #(prefix
                |> string_tree.append(")")
                |> string_tree.append_tree(suffix), buffer) )

            [expr] -> {
                let #(last_line, buffer) = expr
                    |> render_expression(buffer:, prefix:, suffix: string_tree.from_string(")"))
                recursions.End(#(last_line |> string_tree.append_tree(suffix), buffer))
            }

            [expr, ..args] -> {
                let #(prefix, buffer) = expr
                    |> render_expression(buffer:, prefix:, suffix: string_tree.from_string(", "))
                recursions.Continue(#(buffer, prefix, args))
            }
        }
    }
}

fn render_operation(operator, expression1 e1, expression2 e2, buffer buffer, prefix prefix, suffix suffix)
{
    let operator = string_tree.from_string(operator)
    let #(prefix, buffer) =
        render_expression(e1, buffer:, prefix:, suffix: operator)
    render_expression(e2, buffer:, prefix:, suffix:)
}

fn render_expression(expression: SQLExpression, buffer buffer: List(StringTree), prefix prefix: StringTree, suffix suffix: StringTree)
{
    case expression 
    {
        NumericFloatLiteral(value:) ->
            #(prefix
                |> string_tree.append(float.to_string(value)) 
                |> string_tree.append("::FLOAT")
                |> string_tree.append_tree(suffix), buffer)

        NumericIntLiteral(value:) -> 
            #(prefix
                |> string_tree.append(int.to_string(value))
                |> string_tree.append("::INT")
                |> string_tree.append_tree(suffix), buffer)

        VarcharLiteral(value:) ->
            #(prefix
                |> string_tree.append("'")
                |> string_tree.append(value)
                |> string_tree.append("'")
                |> string_tree.append_tree(suffix), buffer)

        FunctionCall(function:, arguments:) -> function
            |> render_function_call(arguments, buffer, prefix, suffix)

        VariableCall(variable:) -> 
            #(prefix
                |> string_tree.append(variable)
                |> string_tree.append_tree(suffix), buffer)

        BinaryOperation(operator: IntDivision, e1:, e2:) ->
            ") / (" |> render_operation(e1, e2, buffer,
                prefix |> string_tree.append("("),
                suffix |> string_tree.prepend(")::INT")
            )

        BinaryOperation(operator: op, e1:, e2:) -> render_operator(op)
            |> render_operation(e1, e2, buffer, prefix, suffix)
    }
}

fn render_operator(operator: SQLOperator)
{
    case operator
    {
        Addition -> " + "
        And -> " AND "
        Concatenate -> " || "
        IntDivision -> " / "
        FloatDivision -> " / "
        Equals -> " = "
        Multiplication -> " * "
        NotEquals -> " <> "
        Or -> " OR "
        Substraction -> " - "
        GreaterOrEqual -> " >= "
        GreaterThan -> " > "
        LessOrEqual -> " <= "
        LessThan -> " < "
        Modulo -> " % "
    }
}

fn render_statement(statement: SQLStatement,indent indent: StringTree, buffer buffer: List(StringTree))
{
    let #(last_line, buffer) = case statement
    {
        Assignment(variable:, expression:) -> expression |> render_expression(
            buffer:,
            suffix: string_tree.from_string(";"),
            prefix: indent |> string_tree.append(variable.name)
                |> string_tree.append(" := ")
        )

        Return(expression:) -> expression |> render_expression(
            prefix: indent |> string_tree.append("RETURN "),
            suffix: string_tree.from_string(";"),
            buffer:,
        )

        Expression(expression:) -> expression |> render_expression(
            prefix: indent,
            suffix: string_tree.from_string(";"),
            buffer:,
        )

    }
    [last_line, ..buffer]
}

fn render_declaration(declarations, indent indent, buffer buffer)
{
    let buffer = buffer |> list.prepend(string_tree.from_string("DECLARE"))
    use #(buffer, #(current, declarations))
        <- recursions.start(#(buffer, declarations))
    case declarations
    {
        [] -> buffer |> list.prepend(
            render_variable(current)
                |> string_tree.prepend_tree(indent)
                |> string_tree.append(";")
        ) |> recursions.End

        [next, ..rest] -> #(buffer |> list.prepend(
            render_variable(current)
                |> string_tree.prepend_tree(indent)
                |> string_tree.append(";")
        ), #(next, rest)) |> recursions.Continue

    }
}

fn render_function(function: SQLFunction, buffer)
{
    let indent = string_tree.from_string("    ")
    let SQLFunction(name:, args:, return_type:, body:, declarations:) = function
    let buffer = buffer
        |> list.prepend(
            string_tree.from_string("DROP FUNCTION " <> name <> " CASCADE;")
        ) |> list.prepend(
            render_function_signature(name, args, return_type)
                |> string_tree.append(" AS $$"),
        )    let buffer = case declarations
    {
        [] -> buffer
        [first, ..rest] -> {
            #(first, rest) |> render_declaration(indent:, buffer:)
        }
    } |> list.prepend(string_tree.from_string("BEGIN"))
    // insert body here 
    let buffer = {
        use buffer, statement <- list.fold(body, buffer)
        statement |> render_statement(indent:, buffer:)
    }

    buffer
        |> list.prepend( string_tree.from_string("END;") )
        |> list.prepend( string_tree.from_string("$$ LANGUAGE plpgsql;") )
}
