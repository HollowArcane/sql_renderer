# sql_renderer
A tool to transpile gleam code into sql code

####!!! Please do not use as it is still in development (like really early development)

The following code
```gleam
import sql_renderer/types

@external(erlang, "sql", "SUBSTRING")
pub fn substring(buffer: types.Varchar, start_pos: types.Integer, len: types.Integer) -> types.Varchar

pub fn fn_mid2(buffer: types.Varchar, start_pos: types.Integer, len: types.Integer) -> types.Varchar
{
    substring(buffer, start_pos, len)
}
```

```sql
CREATE OR REPLACE FUNCTION fn_mid2 (buffer VARCHAR, start_pos INTEGER, len INTEGER) RETURNS VARCHAR AS $$
BEGIN
    RETURN substring(buffer, start_pos, len);
END;
$$ LANGUAGE plpgsql
```

