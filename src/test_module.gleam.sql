CREATE OR REPLACE FUNCTION fn_mid2 (buffer VARCHAR, start_pos INTEGER, len INTEGER) RETURNS VARCHAR AS $$
BEGIN
    RETURN substring(buffer, start_pos, len);
END;
$$ LANGUAGE plpgsql