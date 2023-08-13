CREATE TABLE "recipes" 
    ( id SERIAL PRIMARY KEY NOT NULL UNIQUE,
      name TEXT NOT NULL,
      description TEXT NOT NULL
    )