DEAP Version 2.1
****************

A DATA ENVELOPMENT ANALYSIS PROGRAM

by
Tim Coelli
Centre for Efficiency and Productivity Analysis
School of Economics
University of Queensland
Brisbane, QLD 4072
Australia.
email: t.coelli@economics.uq.edu.au
Web: http://www.uq.edu.au/economics/cepa/


GETTING STARTED:

1. Open the WINDOWS EXPLORER program on your PC.

2. Create a new folder on your hard drive, for example called DEAP, by 
using the File/New/Folder option.

3. Copy ALL the files associated with DEAP into this folder, using
the File/Copy option or by "drag-and-drop".

4. If the files are stored in a DEAP.ZIP file, you will then need
to unzip (extract) the files by double-clicking on the zip file and 
extracting the contents of the zip file into the DEAP folder.


FILES USED BY DEAP

DEAP uses three text files when it conducts an analysis.  These are:
	- a data file (eg. named eg1-dta.txt)
	- an instruction file (eg. named eg1-ins.txt)
	- an output file (eg. named eg1-out.txt)

All of these files are text files.  They can be edited using many programs.
For example, NOTEPAD, WORDPAD, WORD, WORD PERFECT, etc.  
We suggest you use WORDPAD to view and edit these files.


HOW TO RUN THE PROGRAM

To practice running DEAP double-click on the DEAP.EXE file name.  
The program will then ask for an instruction file name.  Type in 
	eg1-ins.txt 
(and hit the RETURN key).  DEAP will only take a few seconds to complete
this small example.  To look at the output file (eg1-out.txt) you then
simply double-click on the eg1-out.txt file name.   


FILE NAMES IN DOS

DEAP is a DOS computer program.  In DOS all file names must satisfy certain
restrictions:
	- no more than 12 characters
	- no more than 3 characters after the period (".")
	- no more than 8 characters before the period
That is, the largest possible file name has the form:
	XXXXXXXX.XXX
Since we use text files, the file name will always have "txt" after the 
period.  That is:
	XXXXXXXX.txt


DATA FILE STRUCTURE

All data, instruction and output files are text files.  In the data file
the output columns are listed first followed by the input columns (left to
right across the file).  If you are doing a cost eff DEA the input price 
columns are listed to the right of these.  If you are doing Malmquist DEA
all observations for year 1 are listed first, then those for year 2, and 
so on down the file.  Note that there should be no column names in the 
data file, only numbers.  For further details see the manual.


CREATING A NEW INSTRUCTION/COMMAND FILE

The easiest way to create a new instruction file is to open 
an existing instruction file (eg. eg1-ins.txt) using WORDPAD and then save
it under a new name (eg. abc2-ins.txt).  This is done by using the File/SaveAs
option in WORDPAD.  Then you can edit the contents of this new file to suit
your new analysis.


CREATING DATA FILES USING EXCEL

Many people store their data in Excel files.  To construct a text file from
an Excel file, open the file in Excel and then use the File/SaveAs option in 
Excel and save it as a text file.  However, be sure to remove any column names 
first - the data file should only contain numbers.


MANUAL

        Coelli, T. (1996) 'A Guide to DEAP Version 2.1: A Data
        Envelopment Analysis (Computer) Program', CEPA Working Paper 96/08,
	  Centre for Efficiency and Productivity Analysis, University of New 
	  England.

A COPY OF THIS GUIDE IS PROVIDED IN THE FILE: DEAP.PDF.  

For more on these DEA methods see Chapters 6, 7 and 10 in Coelli, 
Rao and Battese (1998), An Introduction to Efficiency and Productivity Analysis, 
Kluwer Academic Publishers, Boston.  


LIABILITY

Every effort has been made to ensure that this software is free of errors.  
However, the software is supplied without any warranty as to the 
performance or fitness of the program for any particular purpose.  All 
risk relating to the results and performance of this program is assumed by
the user.  CEPA shall not be liable for any damages resulting from the 
use of this product.



