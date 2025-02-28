# Reproducible research: version control and R

\# INSERT ANSWERS HERE #

# Questions 1, 2 and 3:

The answers to these questions can be found at my logistic growth repository:
https://github.com/user39201/logistic_growth

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

B) To fit a linear model to the data, you can perform a natural log transformation on the data. For this reason, the equation for allometric law of dsDNA is transformed to ln(V) = ln(B) + aln(L). This can be fit to the linear model y = b + mx, and have a linear regression analysis performed upon it.

C) After performing a natural log transformation and a linear regression analysis on the data, my calculated values were as follows:

B = 1181.807 (3 dp), (ln(B) = 7.0748, p_val = 2.28e-10 < 0.05)

a = 1.5152 (p_val = 6.44e-10 < 0.05)

The p value for the regression line was 6.438e-10 < 0.05

All p values obtained were statistically significant.

The values for the exponent and the scaling factor for dsDNA of the viruses were the same in table 2 of the paper (a = 1.52, B = 1182) as was the case from my own analysis (a = 1.5152, B = 1181.807).

D) The code for the replication of the plot is in this repository.

E) A dsDNA virus with a genome length (L) of 300Kb, will have an estimated volume (V) of (1181.807)((300)^1.5152) = 6,697,005.925 nm3 according to the model.

F) Reproducbility is concerned with whether consistent results can be obtained when using the same data and methods by a different set of researchers in a different setting and or time. In comparison, replicability involves whether consistent results can be obtained when the entire study is repeated, including data collection and it's analysis. For this reason, reproducibility is more focussed on the robustness and transparency of results whereas replicability determines how generalizeable the results of a study are, and whether similar studies can be conducted and achieve consistent results in line with the original. 

In this sense, git and Github can be used to improve reproducibility as stored on these repositories should be all of the data, code and files required to perform the same study and achieve the same results by any independant user. They can aid replicability, not only by providing the relevant files required to conduct the study, but also provide a chronology of the changes and modifications that were made to files throughout the study due to version control. Version control means researchers can go back to specific stages of the study and ensure these can be replicated in the future. 

Furthermore, README files are easily accessible and should serve as a place where researchers explain the reason for the study, the methodology and provide instructions that can be used to replicate the study completely. Additionally the data sharing capabilities of Github mean raw data should be easily accessed and obtained by other researchers to either repeat the study, or even conduct their own using the same data. Github can also be used to clarify licenses that can provide easy instruction on how researchers wan't their data and files used. Hence Github contributes to the transparency and also open access of scientific research.

In terms of limitations, git and Github have a few. First there are limits on the sizes of files that can be uploaded therefore this restricts certain data sets from being added to the repositories and therefore access to other researchers. Second, Git doesn't handle dependencies directly, therefore if a project and it's code has such dependencies, these must be indicated by the authors. Third, due to the public nature of Github repositories they can be accessed by anyone. This is advantageous for open access research, however means that sensitive information and data can't be uploaded to these repositories due to the risk of it being accessessed by innapropriate individuals. Fourth, repositories on Github may not be maintained indefinitely and therefore might require researchers to seek other places to store their files, which in itself could introduce error if differences exist between different repositories. 

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
