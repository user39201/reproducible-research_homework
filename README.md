# Reproducible research: version control and R

\# INSERT ANSWERS HERE #

# Question 4:
A) Both graphs begin at (0,0) at the time step where t = 1.
With each time step, the graphs behave differently, moving to different x and y coordinates seemingly randomly.
Despite movement in all directions, both graphs descend the y axis (tend downwards) as time progresses.
For the same period of time, despite the coordinate distance from the starting point being different between the graphs, it appears as though both have the same generale displacement from the original point. The range of values taken on in the x and the y axis appears to be similar, for example my plot1 takes on x values from approxmiately -2, to 3 giving a total range of 5. Similarly, plot2 takes on x values from approximately -1 to 4, also giving a range of 5. Likewise, plot1 takes on y values approximately from 0 to 10.5 and plot2 takes on values approximately from -1 to 8. These total y ranges are more different than the x ranges, but I would argue that comparing the directions of motion of plot1 and plot2, it is clear that plot2 did more 'back tracking' whereby it either rose back up the y axis, or remained at a similar point for multiple time steps. In comparison, plot1 descended down the y axis more continually, with less stalling. Hence despite having different total ranges of distance on the y axis, I believe the displacement between the graphs is again much more similar.

B) A random seed is a fixed starting point from which random number generation can occur. It is essential in reproducibility as it means researcher's code using random number generation, will have a fixed starting point (seed), that will ensure when other researchers run the code, they obtain the same series of numbers from the random number generation. It enables simulations to be reproducible, more transparent and verifiable.

C)

D) 

![Screenshot 2023-12-05 at 16 32 04](https://github.com/user39201/reproducible-research_homework/assets/150145166/83951c5d-c173-4439-9d57-bc3187014ea8)

![Screenshot 2023-12-05 at 16 33 08](https://github.com/user39201/reproducible-research_homework/assets/150145166/a35d30a7-57be-4fb8-a3ff-8ccb17535c12)

# Question 5:

A) The data set has 33 rows and 13 columns

B) 

## Instructions

The homework for this Computer skills practical is divided into 5 questions for a total of 100 points (plus an optional bonus question worth 10 extra points). First, fork this repo and make sure your fork is made **Public** for marking. Answers should be added to the # INSERT ANSWERS HERE # section above in the **README.md** file of your forked repository.

Questions 1, 2 and 3 should be answered in the **README.md** file of the `logistic_growth` repo that you forked during the practical. To answer those questions here, simply include a link to your logistic_growth repo.

**Submission**: Please submit a single **PDF** file with your candidate number (and no other identifying information), and a link to your fork of the `reproducible-research_homework` repo with the completed answers. All answers should be on the `main` branch.

## Assignment questions 

1) (**10 points**) Annotate the **README.md** file in your `logistic_growth` repo with more detailed information about the analysis. Add a section on the results and include the estimates for $N_0$, $r$ and $K$ (mention which *.csv file you used).
   
2) (**10 points**) Use your estimates of $N_0$ and $r$ to calculate the population size at $t$ = 4980 min, assuming that the population grows exponentially. How does it compare to the population size predicted under logistic growth? 

3) (**20 points**) Add an R script to your repository that makes a graph comparing the exponential and logistic growth curves (using the same parameter estimates you found). Upload this graph to your repo and include it in the **README.md** file so it can be viewed in the repo homepage.
   
4) (**30 points**) Sometimes we are interested in modelling a process that involves randomness. A good example is Brownian motion. We will explore how to simulate a random process in a way that it is reproducible:

   - A script for simulating a random_walk is provided in the `question-4-code` folder of this repo. Execute the code to produce the paths of two random walks. What do you observe? (10 points)
   - Investigate the term **random seeds**. What is a random seed and how does it work? (5 points)
   - Edit the script to make a reproducible simulation of Brownian motion. Commit the file and push it to your forked `reproducible-research_homework` repo. (10 points)
   - Go to your commit history and click on the latest commit. Show the edit you made to the code in the comparison view (add this image to the **README.md** of the fork). (5 points)

5) (**30 points**) In 2014, Cui, Schlub and Holmes published an article in the *Journal of Virology* (doi: https://doi.org/10.1128/jvi.00362-14) showing that the size of viral particles, more specifically their volume, could be predicted from their genome size (length). They found that this relationship can be modelled using an allometric equation of the form **$`V = \beta L^{\alpha}`$**, where $`V`$ is the virion volume in nm<sup>3</sup> and $`L`$ is the genome length in nucleotides.

   - Import the data for double-stranded DNA (dsDNA) viruses taken from the Supplementary Materials of the original paper into Posit Cloud (the csv file is in the `question-5-data` folder). How many rows and columns does the table have? (3 points)
   - What transformation can you use to fit a linear model to the data? Apply the transformation. (3 points)
   - Find the exponent ($\alpha$) and scaling factor ($\beta$) of the allometric law for dsDNA viruses and write the p-values from the model you obtained, are they statistically significant? Compare the values you found to those shown in **Table 2** of the paper, did you find the same values? (10 points)
   - Write the code to reproduce the figure shown below. (10 points)

  <p align="center">
     <img src="https://github.com/josegabrielnb/reproducible-research_homework/blob/main/question-5-data/allometric_scaling.png" width="600" height="500">
  </p>

  - What is the estimated volume of a 300 kb dsDNA virus? (4 points)

**Bonus** (**10 points**) Explain the difference between reproducibility and replicability in scientific research. How can git and GitHub be used to enhance the reproducibility and replicability of your work? what limitations do they have? (e.g. check the platform [protocols.io](https://www.protocols.io/)).
