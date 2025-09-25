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
