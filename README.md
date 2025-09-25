# sql_renderer
A tool to transpile gleam code into sql code

#### !!! Please do not use as it is still in development (like really early development)

The following code
```gleam
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
```
turns into
```sql
DROP FUNCTION fn_mid2 CASCADE;
CREATE OR REPLACE FUNCTION fn_mid2 (buffer VARCHAR, start_pos INTEGER, len INTEGER) RETURNS VARCHAR AS $$
DECLARE
    len1 INTEGER;
    my_var1 REAL;
    the_start_pos1 INTEGER;
BEGIN
    my_var1 := 2.6::FLOAT;
    the_start_pos1 := start_pos;
    len1 := 2::INT + len;
    RETURN SUBSTRING(buffer, the_start_pos1, len1);
END;
$$ LANGUAGE plpgsql;
```

