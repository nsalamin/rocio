## ListFasta.txt:
names of Fasta files, id sample, lambda, approximate depth

## depth.txt: 
list of actual depths for each tree
 
## RandTrees/ 
folder with trees, 500 tips
names corresponds to  d_randtree_s_l.tree d being the depth, s the id sample and l lambda
d is = norm if depth=0.5,
	   0.1norm depth =0.1
	   0.5     depth=5

## FASTA/
### WithCoevCAT/
	fasta simulated with CAT gamma =1 and coev model d/s=100 
	25 pairscoevolving 100 sites
	Also in the folder the number of substitution and a folder with figures of the evolving sites along the trees

### WithCoevLG/
	fasta simulated with LG gamma =1 and coev model d/s=100 
	25 pairscoevolving 100 sites
	Also in the folder the number of substitution and a folder with figures of the evolving sites along the trees

###  WithCoevMixture/
        fasta simulated with LG gamma =1, CAT gamma 1 and coev model d/s=100 
        25 pairscoevolving 100 sites, 25 LG 25 CAT
        Also in the folder the number of substitution and a folder with figures of the evolving sites along the trees


## Results/
    some figures with results of plmDCA on FASTA simulated with CoevMixture
