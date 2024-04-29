This R code will analyze data with binary outcomes. The code performs the following: 
1.	Reads a data sheet
2.	Creates factors of variables 
3.	Fits data to a binomial generalized linear model using factors as input
4.	Runs an ANOVADEV on model to determine if there is a significant effect of strain, concentration, or an interaction 
5.	Outputs ANOVADEV results into a spreadsheet with formatted P values 
6.	Calculates expected values, upper, and lower limits for each strain and condition then converts all values to a linear scale and exports all values in a spreadsheet 
7.	Reads a post hoc design matrix then creates a comparison matrix
8.	Performs a hypothesis test on the model with comparisons specified by comparison matrix 
9.	Outputs summary of hypothesis test results into a spreadsheet

To use this code, you will need to create two Excel sheets: your data sheet and a post hoc comparison sheet. You will also need to download the “customfunction.R” file to run the code for statistical analysis because all files call functions from “customfunction.R”. 
The statistical analysis code will output data sheets; therefore, it’s recommended to create a single folder to store all the input and output for each individual data sheet analyzed. Before running any code, make sure your data sheet, the custom functions file, and the statistical analysis file are all in the same folder and that your R workspace is set up to read these files (use the setwd() function).   

Data sheet: 
Data should be organized in columns with the following labels: 
avoided	number	strain	concentration	chemical
 
Each row should represent one plate tested.  

Avoided is the number of worms that avoided a stimulus on the plate. 
Number is the total number of worms that were tested per plate. 
Strain is the name of the worm strain that was tested. 
Concentration is chemical concentration used. 
Chemical is the repellent used. 
Alternatively, you can name these columns different names if it makes more sense to you, but if you do, just make sure you change all the references to it in the code to match. 

Before your post hoc tests:

The first time you run your data in the code, you will not perform a post hoc test. This is because to run post hoc tests, the ANOVADEV test must indicate that there must be a significant effect of whatever you’re comparing. In other words, if the ANOVADEV finds that there is a significant effect of strain but not concentration, then you can run post hoc tests that compare strains but not concentrations. To find out which variables have a significant effect, look at the results of the anovatable file. 

Post hoc design: 

If you are able to perform post hoc tests, then you will need to create a post hoc design matrix. The post hoc design matrix you create will determine the comparisons that the glht function performs when hypothesis testing. You can set up your matrix in excel. In the top row, you should have each condition you’re interested in and in column A you should enter the comparisons you’re interested in making. Fill in the matrix by putting a zero for each condition not included in the comparison. Then fill in the conditions you are comparing with a 1 or negative 1, depending on which you are subtracting from the other.

For example, if I had three strains responding to two different chemical concentrations and I wanted to compare how each strain responded within a chemical concentration, I would create the following matrix. 

comparison	Strain 1 C1	Strain 2 C1	Strain 3 C1	Strain 1 C2	Strain 2 C2	Strain 3 C2
Strain 2 C1 – Strain 1 C1 	-1	1	0	0	0	0
Strain 3 C1– Strain 1 C1	-1	0	1	0	0	0
Strain 3 C1- Strain 2 C1	0	-1	1	0	0	0
Strain 2 C2 – Strain 1 C2 	0	0	0	-1	1	0
Strain 3 C2– Strain 1 C2	0	0	0	-1	0	1
Strain 3 C2- Strain 2 C2	0	0	0	0	-1	1

C1 and C2 represent different chemical concentrations while strain 1, 2, and 3 represent three different worm strains. The results of your post hoc test will be output in a CSV file.

After running everything: 
After you’ve run everything, you should have a folder with the following files: 
•	Raw data (this was in the folder before you ran anything)
•	Custom function R code (this was in the folder before you ran anything)
•	Statistical analysis R code (this was in the folder before you ran anything)
•	ANOVA summary table 
•	Expected values table 
•	Post hoc design table (this will only be in your folder if you found a significant effect of a variable from the ANOVADEV)
•	Post hoc summary table (again, this will only be in your folder if you found a significant effect of a variable from the ANOVADEV)

