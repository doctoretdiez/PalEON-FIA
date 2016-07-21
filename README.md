#### Creating an FIA dataset and estimating forest parameters
## Written by Sean DuBois 9/21/2015
## Revised by Sean DuBois 12/17/2015

## Download software and data
1.	Download PostgreSQL from the [PostgreSQL website] (http://www.postgresql.org/download/) (Note: this SOP written using version 9.4.4)
2.	FIA data can be downloaded from the [US Forest Service website] (http://apps.fs.fed.us/fiadb-downloads/datamart.html)
3.	Download pgfutter from @lukasmartinelli [Github page](https://github.com/lukasmartinelli/pgfutter) Note: this SOP written using version 0.3.2; old versions available under [releases] (github.com/lukasmartinelli/pgfutter/releases). Most likely you are using a 64-bit machine, so download …amd64.exe

## Optional: Set up password file 
4.	A password file allows the user to log into the Postgres database without entering a password every time (works on both the psql command line and within pgAdmin III)
5.	A detailed description for finding and editing the local password file can be found here on the [PostgreSQL website] (http://www.postgresql.org/docs/9.0/static/libpq-pgpass.html)
6.	Multiple lines can be entered into the pgpass.conf file (that you need to add to the correct directory, usually under AppData/Roaming/postgresql/). For example, with a password of “password,” these lines provide access to the database postgres for the user postgres, and access to all databases and users (with this password), respectively:
``` 
localhost:5432:postgres:postgres:password
*:*:*:*:password
```
Note that multiple lines can be included in this file.

## Setting up the program ‘pgfutter’
7.	Move the .exe file for pgfutter to a local directory, and make a copy of the file with the name “pgfutter” (for simplicity). Copy the path to this directory.
8.	Add path for pgfutter to accepted paths. Do this by right clicking on “Computer” and choosing Properties. Then go to Advanced System Settings > Environmental Variables. In the bottom pane, select the variable “Path” and click Edit. Add the copied path after the last semicolon

## Importing FIA data into PostgreSQL
9.	The syntax for pgfutter on Windows is as follows:
```
> pgfutter --pw “password” csv yourcsv.csv
```
NOTE: shouldn’t need quotes around the password. If using custom host, port, or username, type 
```
pgfutter --help 
```
for additional global options to customize.

10.	To import all csv files in a directory, cd to that directory in cmd and enter:
```
> for %f in (*.csv) do pgfutter --pw “password” csv %f
```
11.	The data is imported to the Postgres database under the Schema “import” by default. 
12.	Change the name of this Schema; if dealing with many states, change it to the state abbreviation. This is easiest accomplished by opening PG Admin III and right clicking on the desired schema and selecting “Properties.” If importing multiple states and want to keep each in separate schema, do this between each import. 
13.	Note: Indiana cannot be left as IN, as specifying this schema in a query causes problems as “in” is a command; so this schema should be names ind, and all files within the schema should follow the pattern ‘ind_’ instead is ‘in_’. 
14.	Ensure upload worked correctly by viewing some of the imported tables.

## Alternative import method 
15.	Use a backup file titled ‘FIA_states’ to restore a database containing all state data, generated using the command
```
> pg_dump -U postgres -v FIA_states > FIA_states.sql
```
 This backup also includes the combined plot, tree and survey tables, but not the true coordinates. The backup is currently a zipped file, and needs to be unzipped prior to restoring.
16.	From the command line, create a new database titled FIA_states 
```
> createdb -E UTF8 -U postgres FIA_states
```
Note: these commands were executed using Windows cmd, with the PostgreSQL superuser ‘postgres,’ and a password file.

17.	Within the same directory as the backup file, execute the following command to restore the backed up database to the new database
```
> psql -U postgres -d FIA_states -f FIA_states.sql
```
This database includes state data for the 16 Midwest and Northeast states, in addition to the combined states tables outlined below in the section “Combine similar tables between states.”

## Combine similar tables between states
Note: To run a query in Pg Admin III, click on the “Execute SQL Queries” button and in the new window, enter text and press the “Execute query” button.

18.	Tables needed for analysis include: survey, tree, & plot (the actual plot data links only to column ‘cn’ in PLOT). Create a schema for the output data (“fiaout”) and execute the query in fiaout_tables_creation.txt.
19.	Use the text in output_combine.txt to combine all necessary tables. The query runs most efficiently with only 1 execute statement, so it is advised to choose only one EXECUTE statement within the loop. Essentially, this query is executed 3 separate times, once for each table of interest. Note the above step creates the table using CT data, while this step adds all states except CT. If not using CT data, replace with a new state and update both queries.
20.	Ensure that no states were added multiple times by running Remove_duplciates.txt and comparing row counts to the original table. They should be identical.

## Merge FIA data with true location data
21.	In order to join two tables, all selected columns names must not have duplicates. Change the “cn” column from the actual coordinates table (nrs_actual_coords_forested_plots) to “cnFIA.”
22.	Use the text in FIA_Plot_Merge.txt to create a plot table with the real lat/lon coordinates. 

## Generate table to be used in R script
Use the text in Query4.txt to create a table needed for the R script control_file . Note this query pulls the correct lat/lon coordinates, and relabels them at ‘lat’ and ‘lon.’

## Estimate Density, DBH, Basal Area, and Biomass 
Use the R script control_file.Rmd to import FIA data from SQL (or alternatively, read in the csv file full_fia_long.csv located at data/output/), estimate forest parameters at the tree and species level, and generate output rasters. To run, this file requires the location data file plt_cn_values.csv to be located at data/plt_location/.

Note that the R scripts used to create those sourced within control_file.Rmd are located within the directory R_scripts/original_scripts/. If you wish to manipulate the code as a whole, you can use these 'rough' scripts, and then will need to execute the SQL queries located within the text files Query3.txt and Query4.txt within postgreSQL.