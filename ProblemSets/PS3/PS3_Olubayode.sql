
-- a) Read the file by opening SQLite3 and Create a New Database
sqlite3 Insurance.db

-- Create a Table
CREATE TABLE insurance (
    policyID INTEGER,
    statecode TEXT,
    county TEXT,
    eq_site_limit REAL,
    hu_site_limit REAL,
    fl_site_limit REAL,
    fr_site_limit REAL,
    tiv_2011 REAL,
    tiv_2012 REAL,
    eq_site_deductible REAL,
    hu_site_deductible REAL,
    fl_site_deductible REAL,
    fr_site_deductible REAL,
    point_latitude REAL,
    point_longitude REAL,
    line TEXT,
    construction TEXT,
    point_granularity INTEGER
);
-- import the CSV file into the table
.mode csv
.import FL_insurance_sample.csv insurance

-- b) Print out the first 10 rows of the data set
SELECT * FROM insurance LIMIT 10;

-- c) List which(unique) counties are in the sample
SELECT DISTINCT county FROM insurance;

-- d) Compute the average property appreciation from 2011 to 2012
SELECT AVG(tiv_2012 - tiv_2011) AS average_appreciation FROM insurance;

-- e) Create a frequency table of the construction variable
SELECT construction, COUNT(*) AS total, 
       COUNT(*) * 100.0 / (SELECT COUNT(*) FROM insurance) AS percentage 
FROM insurance 
GROUP BY construction;
