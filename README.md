# Haskell plagiarism detection

The objective of this project is creating a software that is precise in plagiarism detection in Haskell codes. The project relies cardinally on the [paper of M. L. Kammer](https://www.macs.hw.ac.uk/~jh2054/downloads/marnixkammer-msc.pdf).

## How to use it

After running the code it needs a path to the file with the following infos:
- In the first line we have to give the function names on which we want to run the algorithm
- Then in each line we give the paths of the files which have to be matched

## About the algorithm

The program is based on call-graph matching, so firstly we have to construct the call-graph to each function we need to match in the codes. After that, we need to find the shortest path from one call-graph to another. The program uses A* search to get this shortest path, and then calculates the precentage of matching by finding all differences between the original graph and the graph the A* search gave back (the end of the shortest path).
