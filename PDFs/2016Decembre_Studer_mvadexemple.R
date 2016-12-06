
## Loading the library
library(TraMineR)
##Loading example data
data(mvad)
## Defining our labels
mvad.lab <- c("employment", "further education", "higher education", "joblessness", "school", "training")
## Defining our short labels
mvad.shortlab <- c("EM", "FE", "HE", "JL", "SC", "TR")

##Creating the state sequence object.
mvad.seq <- seqdef(mvad[, 17:86], states = mvad.shortlab, labels = mvad.lab, xtstep = 6)


## Plotting individual sequences
seqiplot(mvad.seq, border=NA)


## Plotting individual sequences
seqIplot(mvad.seq)

## Frequent sequences
seqfplot(mvad.seq, border=NA)

## Parallel coordinates plot by result at the end of compulsory schooling
seqpcplot(mvad.seq, group=mvad$gcse5eq, border=NA)

## Chronograms
seqdplot(mvad.seq, group = mvad$gcse5eq, border = NA)

## Computing distances using LCS (based on the Longuest Common Subsequence)
lcs <- seqdist(mvad.seq, method="LCS")

## Hierarchical clustering of the data
hc <- hclust(as.dist(lcs), method="ward")

## Dendrogram
plot(hc)


## Extract clustering in xxx groups
k <- cutree(hc, )

## Plot the result
seqdplot(mvad.seq, group=k)

## The mvad dataset contains 5 region dummies from which we
## derive the factor `region' with the following code
region <- character(nrow(mvad))

for(r in c("Belfast", "N.Eastern", "Southern", "S.Eastern", "Western")){
	region[mvad[,r]=="yes"] <- r
}
mvad$region <- factor(region)

## Regression tree
st <- seqtree(mvad.seq~region+funemp, data=mvad, diss=lcs)

## Displaying the result
seqtreedisplay(st, type="d", border=NA)







