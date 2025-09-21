
import sql_renderer/types

@external(erlang, "sql", "SUBSTRING")
pub fn substring(buffer: types.Varchar, start_pos: types.Integer, len: types.Integer) -> types.Varchar

pub fn fn_mid2(buffer: types.Varchar, start_pos: types.Integer, len: types.Integer) -> types.Varchar
{
    substring(buffer, start_pos, len)
}
