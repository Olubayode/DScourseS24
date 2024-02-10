#!/bin/sh

# Download the file
wget http://spatialkeydocs.s3.amazonaws.com/FL_insurance_sample.csv.zip

# List the contents of the current directory
ls

# Unzip the downloaded file
unzip FL_insurance_sample.csv.zip

# Remove unwanted files
rm -rf __MACOSX
rm -f FL_insurance_sample.csv.zip

# Check the file size
ls -al --block-size=MB FL_insurance_sample.csv

# View the first 5 lines of the CSV file
head -5 FL_insurance_sample.csv

# Count the number of lines in the file
wc -l FL_insurance_sample.csv

# Fix the End-Of-Line format for Linux
dos2unix -c mac FL_insurance_sample.csv

# Verify the fix by viewing the first 5 lines again
head -5 FL_insurance_sample.csv

# Finally, count the lines again to confirm correction
wc -l FL_insurance_sample.csv

