---
title: Sexual dimorphism in transcriptomic response to infection
tags: []

date: "2018-01-01T00:00:00Z"

# Optional external URL for project (replaces project detail page).
external_link: ""

image:
  caption:
  focal_point: Smart
---
{{< figure src="Screenshot_app_becca.png" title="" >}}

While I was in the laboratory of Brian Lazzaro in Cornell and collaborating with the laboratory of Nicolas Buchon, we studied the sexual dimorphism in gene expression 8 hours after the start of a bacterial infection (<i>Providencia rettgeri</i>) or a wound. Part of the work is pushied in [Duneau et al. BMC Biology 2017](Duneau_BMCBiology_2017.pdf)


* Thanks to the skills of Nicolas Buchon, we were able to have two sets of data, one where we had the whole body and one where he quickly removed the gonads before we could grind the carcass in Tryzol. He tried to minimize it, but some tracheal tissues may have been removed in the process.  


* We had 3 replicated experiment (different mornings) and each sample is done either with 25 males or 25 females.


* I performed the analysis with R using notably the package [DEseq2](http://www.bioconductor.org/packages/release/bioc/vignettes/DESeq2/inst/doc/DESeq2.html)

Navigate through the <i>Shiny</i> application developed with [Becca Belmonte](http://reganlab.bio.ed.ac.uk/people) (Regan lab, Univ. of Edinburgh) : [RNAseq from Duneau et al BMC Biology 2017](https://david-duneau.shinyapps.io/RNAseq_Sex_Dim_Droso_8h_Prett/)

You will be able to:
* Compare the list of genes implicated in the response to wound, to infection between sexes
* Compare the list of genes constitutively expressed in males and in females
* Compare the list of genes expressed when we take or not the reproductive tissues in the samples.
