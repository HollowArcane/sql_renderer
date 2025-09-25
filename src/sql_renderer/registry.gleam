import gleam/list
import gleam/int
import gleam/result
import sql_renderer/renderer.{type SQLExpression, type SQLType, SQLVariable, VariableCall}
import gleam/dict.{type Dict}

pub type Registry
{
    Registry(
        constants: Dict(String, SQLExpression),
        variable_counter: Dict(String, Int),
        variable_declarations: Dict(String, SQLType),
    )
}

pub fn new()
{ Registry(constants: dict.new(), variable_counter: dict.new(), variable_declarations: dict.new()) }

pub fn get_variable(registry: Registry, name: String)
{ registry.variable_declarations |> dict.get(name) }

pub fn write_variable(registry: Registry, name: String, type_: SQLType)
{
    let name_count = registry.variable_counter |> dict.get(name) |> result.unwrap(0)
    let name_count = name_count + 1
    let var_name = name <> int.to_string(name_count)
    #(Registry(
        variable_counter: registry.variable_counter
            |> dict.insert(name, name_count),
        variable_declarations: registry.variable_declarations
            |> dict.insert(var_name, type_),
        constants: registry.constants
            |> dict.insert(name, VariableCall(var_name))
    ), SQLVariable(var_name, type_))
}

pub fn compute_variable_declarations(registry: Registry)
{
    use #(name, type_) <- list.map(
        registry.variable_declarations |> dict.to_list
    )
    SQLVariable(name:, type_:)
}

pub fn get_constant(registry: Registry, name)
{ registry.constants |> dict.get(name) }

pub fn write_constant(registry: Registry, name, expression: SQLExpression)
{
    Registry(..registry,
        constants: registry.constants |> dict.insert(name, expression)
    )
}
