# SQL-TO-S-SQL

## What is it?

The SQL-TO-S-SQL parser package allows to:
- go from an SQL query to an S-expression type of SQL query
- go from an S-expression type of SQL query to an SQL query
- go from an S-SQL type of query to a predicate network
- and go from a predicate network to an S-SQL query

It is written in LISP. 

It uses a library called Postmodern working with Postgresql. Postmodern is a LISP library allowing the use of Postgresql in a LISP code. 

To test the different functions, a test file is also available with some existing examples. It is important to note that I use the Geoquery SQL database. I converted it in a Postgresql database. To use it follow the following steps :
1. Install PostgreSQL and pgAdmin
2. Create a database like "geo
3. Right-click on the newly created database and choose "Restore"
4. In the "Restore" window, import the geo.sql (from the DB-files folder) and click on "Restore"

The Geoquery for SQL dataset is found on GitHub at the following link: https://github.com/jkkummerfeld/text2sql-data

I also made some tests with the IMDB movies database.

## The predicates and their arguments

The postmodern parser package makes use of what we will call SQL predicates. SQL predicates are based on the SQL keywords used in Postmodern. The SQL predicates we use don't include the non-consensual commands (commands that PostgreSQL may use but not all SQL systems do).

The following tables list all the SQL query commands that consensually exist. Each SQL keyword command has a predicate equivalent. For each of these predicates, we list their possible arguments (they don't necessarily appear in a given query). Note that all given variables are indicative and only made for this explanation. The code doesn't use fixed variables. Some arguments have a -x suffix. When they do, it means that we can find multiple arguments of the same type or that call for the same predicate or type of predicate in a query.
