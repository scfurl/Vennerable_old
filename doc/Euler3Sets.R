### R code from vignette source 'Euler3Sets.Rnw'

###################################################
### code chunk number 1: Euler3Sets.Rnw:1-7 (eval = FALSE)
###################################################
## makeme <- function() {
## 	setwd("C:/JonathanSwinton/Vennerable/pkg/Vennerable/inst/doc")
## 	library(weaver);
## 	Sweave(driver=weaver,"Euler3Sets.Rnw",stylepath=FALSE,use.cache=FALSE)
## }
## makeme()


###################################################
### code chunk number 2: Euler3Sets.Rnw:45-51
###################################################
library(Vennerable)
library(xtable)
Eclass <-EulerClasses(n=3)
Ehave3 <- subset(Eclass,SetsRepresented==3 , -SetsRepresented)
Ehave <- subset(Eclass, ESignature==ESignatureCanonical,-ESignatureCanonical)



###################################################
### code chunk number 3: Euler3Sets.Rnw:55-57
###################################################
print(xtable(Ehave,digits=0),size="small"
)


###################################################
### code chunk number 4: Euler3Sets.Rnw:60-84
###################################################
E3List <- lapply(Ehave3$ESignature,function(VS){
	Weights <- t(Ehave3[Ehave3$ESignature==VS,2:8])[,1]
	Weights["000"] <- 0
	Weights <- Weights[order(names(Weights))]
	Weights
})
names(E3List) <- Ehave3$ESignature

efails <- sapply(names(E3List),function(x) {
	V <- Venn(Weight=E3List[[x]],SetNames=LETTERS[1:3])
	res <- try(compute.Venn(V))
	if (inherits(res,"try-error")) {
		tres <- TRUE
	} else {
		grid.newpage()
		plot(res)
		tres <- FALSE
	}
	tres
})
names(efails) <- names(E3List)

efails[efails]



