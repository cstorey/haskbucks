DO $$
  DECLARE r record;
BEGIN
  FOR r IN SELECT table_schema, table_name FROM information_schema.tables
    WHERE table_type = 'BASE TABLE' AND table_schema = 'public' AND table_name = 'coffee_logs'
  LOOP
    EXECUTE 'TRUNCATE TABLE ' || quote_ident(r.table_schema) || '.' || quote_ident(r.table_name);
  END LOOP;
END
$$;
