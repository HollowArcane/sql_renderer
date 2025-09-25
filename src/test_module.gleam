import sql_renderer/types

@external(erlang, "sql", "SUBSTRING")
pub fn substring(buffer: types.Varchar, start_pos: types.Integer, len: types.Integer) -> types.Varchar

pub fn fn_mid2(buffer: types.Varchar, start_pos: types.Integer, len: types.Integer) -> types.Varchar
{
    let my_var: types.Real = 2.6
    let the_start_pos: types.Integer = start_pos
    let len: types.Integer = 2 + len

    substring(buffer, the_start_pos, len)
}
