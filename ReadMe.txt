1- you need to install https://www.cs.york.ac.uk/aig/sw/gobnilp/.
2- plot-graph (1).R is a file that uses functions from plot-graph-functions (1).R to compare the learned BN with the actual one.
3- main.R is the main project that uses functions from random_Matrix.R to generate BN model and missing data from the generated BN model.
4- scoring2 and gobnilp.set files should be in the same folder with the data to be able to get the adjacency matrix of learned BN model.