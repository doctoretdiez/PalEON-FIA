# Creating an FIA dataset and estimating forest parameters

This code generates a set of data outputs from FIA data obtained fom the [US Forest Service DataMart](http://apps.fs.fed.us/fiadb-downloads/datamart.html), and generates biomass estimates using three procedures.

## Revision History

* Written by [Sean DuBois](http://github.com/sgdubs) 2015/09/21
* Revised by Sean DuBois 2016/07/15

## Instructions for using this repository

### Download software and data
1.	Download PostgreSQL from the [PostgreSQL website](http://www.postgresql.org/download/) (Note: this SOP written using version 9.4.4)
2.	FIA data can be downloaded from the [US Forest Service website](http://apps.fs.fed.us/fiadb-downloads/datamart.html).  Download the states you need.
3.	Download `pgfutter` from `lukasmartinelli` [Github page](https://github.com/lukasmartinelli/pgfutter) Note: this SOP written using version 0.3.2; old versions available under [releases](github.com/lukasmartinelli/pgfutter/releases). Most likely you are using a 64-bit machine, so download the files ending in `amd64.exe`.  `pgfutter` is a commandline tool to import CSV and JSON files into PostgreSQL.

## Optional: Set up password file 
1.	A password file allows the user to log into the Postgres database without entering a password every time (works on both the psql command line and within pgAdmin III)
2.	A detailed description for finding and editing the local password file can be found on the [PostgreSQL website](http://www.postgresql.org/docs/9.0/static/libpq-pgpass.html)
3.	Multiple lines can be entered into the pgpass.conf file (that you need to add to the correct directory, usually under `AppData/Roaming/postgresql/`). For example, with a password of `password`, these lines provide access to the database `postgres` for the user `postgres` (the administrator), and access to all databases and users (with this password), respectively:

``` 
localhost:5432:postgres:postgres:password
*:*:*:*:password
```
Note that multiple lines can be included in this file.

## Setting up the program `pgfutter`

1.	Move the downloaded `.exe` file (for example `pgfutter_windows_amd64.exe`) to a new directory somewhere on your computer.
2. You will be executing the file from the command line, so you'll want to shorten then name. Rename the file `pgfutter.exe`
3. To add this directory to your computer's `PATH`: 
  1. Copy the directory location
  2. Right click `Computer` (or `This PC`) in your File Manager and choose `Properties`. 
  3. Go to Advanced System Settings > Environmental Variables. In the bottom pane, select the variable "Path" and click Edit. 
  4. Add the copied path after the last semicolon
  
NOTE:  Changes to the System `PATH` will only take effect when a Commandline window is re-opened.  You will not be able to access `pgfutter` from any windows that are currently open.

## Importing FIA data into PostgreSQL

The syntax for `pgfutter` on Windows is as follows (once you've opened a command line window using `cmd`):

```
> pgfutter --pw password csv yourcsv.csv
```

**Note**: If you've set a unique password, you'll replace the word `password` with your own. If using custom host, port, or username, type:

```
pgfutter --help 
```

for additional global options to customize.

To import all csv files in a directory, `cd` to that directory in `cmd` and enter:

```
> for %f in (*.csv) do pgfutter --pw password csv %f
```

1.	The data is imported to the Postgres database under the Schema “import” by default. 

2.	Change the name of this Schema; if dealing with many states, change it to the state abbreviation. This is easiest accomplished by opening PG Admin III and right clicking on the desired schema and selecting “Properties.” If importing multiple states and want to keep each in separate schema, do this between each import. 

3.	Note: Indiana cannot be left as IN, as specifying this schema in a query causes problems as “in” is a command; so this schema should be names ind, and all files within the schema should follow the pattern ‘ind_’ instead is ‘in_’. 
4.	Ensure upload worked correctly by viewing some of the imported tables.

## Alternative import method 

Use a backup file titled ‘FIA_states’ to restore a database containing all state data, generated using the command

```
> pg_dump -U postgres -v FIA_states > FIA_states.sql
```

This backup also includes the combined plot, tree and survey tables, but not the true coordinates. The backup is currently a zipped file, and needs to be unzipped prior to restoring.

From the command line, create a new database titled FIA_states 

```
> createdb -E UTF8 -U postgres FIA_states
```

Note: these commands were executed using Windows cmd, with the PostgreSQL superuser ‘postgres,’ and a password file.

Within the same directory as the backup file, execute the following command to restore the backed up database to the new database

```
> psql -U postgres -d FIA_states -f FIA_states.sql
```

This database includes state data for the 16 Midwest and Northeast states, in addition to the combined states tables outlined below in the section “Combine similar tables between states.”

## Combine similar tables between states
Note: To run a query in Pg Admin III, click on the “Execute SQL Queries” button and in the new window, enter text and press the “Execute query” button.

1.	Tables needed for analysis include: survey, tree, & plot (the actual plot data links only to column ‘cn’ in PLOT). Create a schema for the output data (“fiaout”) and execute the query in fiaout_tables_creation.txt.
2.	Use the text in output_combine.txt to combine all necessary tables. The query runs most efficiently with only 1 execute statement, so it is advised to choose only one EXECUTE statement within the loop. Essentially, this query is executed 3 separate times, once for each table of interest. Note the above step creates the table using CT data, while this step adds all states except CT. If not using CT data, replace with a new state and update both queries.
3.	Ensure that no states were added multiple times by running Remove_duplciates.txt and comparing row counts to the original table. They should be identical.

## Create SQLite database with data needed for biomass data product (new step for biomass product production)

1.      Run `biomass_product_workflow/read_postgres.sh` to create the Postgres database (code set up for a Linux machine).
2.      Create an SQLite database containing only the plot, survey, and tree tables using `biomass_product_workflow/create_sqlite_db.R`.
3.      Andria is now set up to rerun the queries to include older survey cycles. This is the current status as of 12/14/17.


## Merge FIA data with true location data
1.	In order to join two tables, all selected columns names must not have duplicates. Change the “cn” column from the actual coordinates table (nrs_actual_coords_forested_plots) to “cnFIA.”
2.	Use the text in FIA_Plot_Merge.txt to create a plot table with the real lat/lon coordinates. 

## Generate table to be used in R script
Use the text in Query4.txt to create a table needed for the R script control_file . Note this query pulls the correct lat/lon coordinates, and relabels them at ‘lat’ and ‘lon.’

## Estimate Density, DBH, Basal Area, and Biomass 
Use the R script control_file.Rmd to import FIA data from SQL (or alternatively, read in the csv file full_fia_long.csv located at data/output/), estimate forest parameters at the tree and species level, and generate output rasters. To run, this file requires the location data file plt_cn_values.csv to be located at data/plt_location/.

Note that the R scripts used to create those sourced within control_file.Rmd are located within the directory R_scripts/original_scripts/. If you wish to manipulate the code as a whole, you can use these 'rough' scripts, and then will need to execute the SQL queries located within the text files Query3.txt and Query4.txt within postgreSQL.
