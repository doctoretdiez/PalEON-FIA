# This code takes the fiaout.tree, fiaout.survey and fiaout.plot tables from the postgres database and creates a (much smaller) SQLite database.
# This code was run on Chris Paciorek's Linux workstation as the postgres user

sudo su - postgres
psql

# now in psql
create database fia tablespace dbspace;
grant all privileges on database fia to paciorek;
\quit

psql -d fia -f /tmp/FIA_states.sql

psql
\c fia
# next steps ensure tables can be found 
grant all privileges on schema fiaout to paciorek; 
grant all privileges on all tables in schema fiaout to paciorek;
\quit



