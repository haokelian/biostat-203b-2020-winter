*Haoke Lian*

### Overall Grade: 101/110

### Quality of report: 10/10

-   Is the homework submitted (git tag time) before deadline? 

    Yes. `Jan 24, 2020, 11:54 PM PST`.

-   Is the final report in a human readable format html? 

    Yes. `html` file. 

-   Is the report prepared as a dynamic document (R markdown) for better reproducibility?

    Yes. `Rmd`.

-   Is the report clear (whole sentences, typos, grammar)? Do readers have a clear idea what's going on and how are results produced by just reading the report? 

	  Include questions followed by answers. 


### Correctness and efficiency of solution: 56/60

-   Q1 (10/10)

-   Q2 (18/20)

    
    \#4. (-2 pts) Your solution counts the number of occurrences `HISPANIC` in the file. But the question asks for the number of **unique** Hispanic patients. Count the number of Hispanic patients with unique `SUBJECT_ID`.
    
-   Q3 (15/15)

    
-   Q4 (13/15)

	  \#3. (-2 pts) Table looks crude. Use `kable` to print the table in the given format. 
	
	    
### Usage of Git: 10/10

-   Are branches (`master` and `develop`) correctly set up? Is the hw submission put into the `master` branch? 

    Yes. 

-   Are there enough commits? Are commit messages clear? 

    20 commits for hw1. 

          
-   Is the hw1 submission tagged? 

    Yes. `hw1`. 

-   Are the folders (`hw1`, `hw2`, ...) created correctly? 

    Yes.
  
-   Do not put a lot auxiliary files into version control. 

	  Yes. 
	  
### Reproducibility: 8/10

-   Are the materials (files and instructions) submitted to the `master` branch sufficient for reproducing all the results? Just click the `knit` button will produce the final `html` on teaching server? (-2 pts)

	  Clicking the `knit` button does not produce the final `html` on the teaching server.  
	  Do not `cd ~`. The current directory (where `hw1sol.Rmd` resides) should contain `pride_and_prejudice.txt` that was downloaded using `curl`. 
	  
-   If necessary, are there clear instructions, either in report or in a separate file, how to reproduce the results?

    Yes.

### R code style: 17/20

-   [Rule 3.](https://google.github.io/styleguide/Rguide.xml#linelength) The maximum line length is 80 characters. (-1 pt)

    Some violations:
      - `autoSim.R`: line 16 

-   [Rule 4.](https://google.github.io/styleguide/Rguide.xml#indentation) When indenting your code, use two spaces.

-   [Rule 5.](https://google.github.io/styleguide/Rguide.xml#spacing) Place spaces around all binary operators (=, +, -, &lt;-, etc.). 	
	
-   [Rule 5.](https://google.github.io/styleguide/Rguide.xml#spacing) Do not place a space before a comma, but always place one after a comma. (-2 pts)

    Some violations:
      - `runSim.R`: lines 26, 42, 44, 53
      - `table.R`: lines 38, 54

-   [Rule 5.](https://google.github.io/styleguide/Rguide.xml#spacing) Place a space before left parenthesis, except in a function call.

-   [Rule 5.](https://google.github.io/styleguide/Rguide.xml#spacing) Do not place spaces around code in parentheses or square brackets.
