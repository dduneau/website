---
title: Within-host dynamics and disease outcomes
summary: Infection outcome depends on the success of the parasite within its host. With theoretical and empirical approaches, we study what influences pathogen dynamics and its implication on symptoms.
tags: ["WHD","Current"]

date: "2019-01-01T00:00:00Z"

# Optional external URL for project (replaces project detail page).
external_link: ""

image:
  caption:
  focal_point: Smart
---

This part of my post-doc can be seen as an extension of my "step-by-step" approach to infection, which was the heart of my thesis. I took advantage of the tools offered by <i>D. melanogaster</i> to study in detail the step were pathogens proliferate within their host. I studied the impact of the variations during this step on the outcome of the infection [Duneau et al. 2017](Duneau_eLife_2017).
Within a population of sick hosts, individuals are not equal when facing diseases. This variation, which sometimes results in the survival or death of hosts, is present even when controlling for the host's genetics and environment. We studied the role of the step of proliferation in mortality variation. Thanks to a monitoring over time of bacterial populations within the hosts of the same population and using modeling tools ("mixture models"), we were first able to describe the phases of the proliferation and propose measures to characterize it. 

* Time to control the infection (Tc)
* Pathogen Load Upon Death (PLUD)
* Set-Point Pathogen Load (SPPL)

By modifying the immune characteristics of the host, we were able to determine the relative roles of the host and the parasite in these phases and we established that the variation in the probability of survival depends on the variation in a key parameter of the infectious process: the time that the hosts take to control bacterial proliferation. A difference of a few hours in this control time can make the difference between survival and death. 

{{< figure src="figure_experimental_WHD.jpg" title="Description of the dynamics of intra-host bacterial proliferation.">}}

a- Bacterial load proliferation within <i>Drosophila</i> using a population method. Eight genetically identical hosts are sacrificed every hour to establish the distribution of bacterial load with the population of infected individuals at a given time. In red, the hosts that are likely to have controlled the proliferation. b- Model of infection dynamics. The bacterial population initially proliferate without host control (the black line Growing corresponds to exponential growth <i>in vitro</i>). Then, the immune system becomes effective enough to control the proliferation. This time to control (Tc) is variable between hosts due to a variance in the speed d'activation of the immune system. If the exponential growth of the bacterial population is controlled before to reach a threshold called « tipping point ", the host will control the bacterial population at a density called "Set Point" Bacterial Load (SPBL or SPPL), the disease will then be chronic. If the control is done after the " tipping period » is reached, the infection cannot be controlled, the infection arrives in the terminal phase and the host will die when the bacterial population will reach BLUD (Bacterial Load Upon Death) or more generally the PLUD (Pathogen Load Upon Death). Thus the stage of proliferation within the host is characterized by three phases: initial, resolution, and chronicity (or terminal)." 

In collaboration with [Jean-Baptiste Ferdy](https://edb.cnrs.fr/annuaire/jean-baptiste-ferdy/), we have used theoretical model and experimental approach to define more specifically the parameters of the infections and determine their role in the two strategies to survive an infection (i.e. controlling the infection - resistance- versus the tolerance of the damage linked to the infection - disease tolerance) [Lafont et al. bioRxiv 2021](https://www.biorxiv.org/content/10.1101/2021.10.19.464998v2.external-links.html). This work aims to finely describe the dynamics of bacteria interacting with the hosts, understand appropriately the parameters, and to compare dynamics statistically.

A continuation of this work allowed us to show that our approach could also be used to characterize the impact of mutations in bacterial pathogens on infection outcomes [Faucher et al., 2021](Faucher_mBio_2020.pdf). The bacterium <i>Xenorhabdus nematophila</i> is in symbiosis with a nematode (<i>Steinernema carpocapsae</i>) to kill insect hosts. The bacterium, carried by the nematode, kills the host and waits until the nematode has reproduced in the host's corpse to reassociate with its vector. During the course of an infection, selection alternately favours two morphs, one that allows the success of the infection and one advantaged in the corpse. The mutation responsible for this second morph has been identified as a mutation in the Lrp gene, a mutation that generally gives an advantage in the stationary phase of growth (mutation called GASP). Our study uses <i>Drosophila</i>'s genetic tools  and the statistical method we developed to characterize the impact of different types of Lrp mutations on infections and understand the characteristics that selected them. We show that GASP mutants are all less virulent but that only missense mutations make bacteria more sensitive to the immune system during proliferation at the beginning of infection. The number of bacteria at the time of death (PLUD) is not affected, suggesting that the toxicity of the bacteria is not altered by the mutation, and that only differences in the ability to grow in the host are responsible for differences in virulence. We show, on the other hand, that the better post-mortem growth of mutants correlates negatively with their virulence. This study introduces a new approach to studying the effects of bacterial mutations in infection, an approach that will be crucial in my future research to characterize experimentally bacterial pathogens.
