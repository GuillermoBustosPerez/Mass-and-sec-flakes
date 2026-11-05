What secondary flakes tells us about flake fracture and the prediction
of flake mass
================
Guillermo Bustos-Pérez $^{1,2,3}$, Shannon P. McPherron $^{1}$

``` r
library(tidyverse); library(caret); library(kableExtra)
Digit_data2 <- read.csv("../Data/Data.csv")
Prim_Flakes <- Digit_data2 %>% filter(Flake_type == "Primary")
Sec_Flakes <- Digit_data2 %>% filter(Flake_type == "Secondary")
```

 

$^{(1)}$ Departament of Human Origins, Max Planck Institute for
Evolutionary Anthropology, Leipzig.  
$^{(2)}$ Institut Català de Paleoecologia Humana i Evolució Social
(IPHES-CERCA), Zona Educacional 4, Campus Sescelades URV (Edifici W3),
43007 Tarragona, Spain.  
$^{(3)}$ Universitat Rovira i Virgili, Departament d’Història i Història
de l’Art, Avinguda de Catalunya 35, 43002 Tarragona, Spain.

<div align="justify">

 

## **Abstract**

Flakes are the primary component of lithic assemblages. Understanding
the relationship between variables controlled by past knappers and flake
mass is fundamental for reduction studies, understanding aspects of
skill, learning, and technological design of stone tools. Mechanical
flaking experiments indicate that platform depth (PD), exterior platform
angle (EPA), and angle of blow (AOB) are the main determinants of flake
mass and morphology. However, most of these mechanical experiments have
been carried out using glass cores without scars from previous removals,
whereas most archaeological flakes have complex scar patterns. Here we
evaluate how well a model trained on primary flakes extends to flakes
with scars from previous removals for the prediction of mass using the
same mechanical flaking framework. We flaked glass hemisphere cores with
varying platform depths and angles of blow (0º and 20º). Following these
initial removals, flakes capturing the previous scars were removed (here
termed “secondary flakes”). This allowed us to generate an experimental
collection of 179 primary flakes and 154 secondary flakes. Two
approaches were used to determine how well the model generalizes to
secondary flakes. First, a multiple linear regression was trained on the
primary flakes and was then used to make flake mass predictions for the
secondary flakes. A second multiple linear regression was trained with
the secondary flakes. Both multiple linear regressions used the platform
depth, the angle of blow and fracture angle as predictors. In both
cases, the correlation and accuracy of the model derived on primary
flakes decreased when applied to secondary flakes with the model tending
to overestimate flake mass. The results suggest that current efforts to
improve flake mass prediction through improvements of the existing
variable set (PD, EPA and AOB) will continually over-estimate mass.

 

**Key words:** flake formation; lithic analysis; experimental
archaeology; controlled experiments.

 

## **1. Introduction**

Lithic flakes are present since the earliest stone tool industries
\[[1](#ref-braunEarliestKnownOldowan2019)–[4](#ref-semawWorldsOldestStone2000)\],
up until modern stone tool users
\[[5](#ref-casamiquelaTemasPatagonicosInteres1978a)–[11](#ref-weedmanEthnoarchaeologicalStudyHafting2006a)\],
and they are the main component of Paleolithic stone tools assemblages.
The production of flakes informs about many aspects of past hominids:
motor control and cognitive abilities
\[[12](#ref-kivellAustralopithecusSedibaHand2011)–[15](#ref-tothOldowanReassessedClose1985)\];
possible learning skills and levels of technicality among past groups
and individuals
\[[16](#ref-geribasWhatNoviceKnappers2010)–[19](#ref-pargeterTestingEffectLearning2023)\];
the evolution, internal variability and adaptation of different lithic
techno-complexes through time and space
\[[20](#ref-debenathHandbookPaleolithicTypology1994)–[22](#ref-rezekTwoMillionYears2018)\];
and site occupation patterns and raw material economy
\[[23](#ref-andrefskyRawMaterialAvailabilityOrganization1994)–[25](#ref-kuhnFormalApproachDesign1994)\].
Therefore, understanding the variables that determine flake size and
morphology is essential.

 

Understanding what determines flake mass in particular and how it
relates to features controlled by the knapper has two major applications
for the study of the past: technological design and reduction studies.
In technological design it can inform on what ranges of mass were
targeted and how that mass was distributed among the flake (for example,
very thick and asymmetric or flat flakes with acute edges). The
distribution of mass and its relationship with other variables also
affects studies focusing on knappers skill. Studies focusing on flake
production according to skill level
\[[18](#ref-nonakaHowStoneKnappers2010)\] showed that expert knappers
are capable of detaching flakes from surfaces making an angle close to
90º, resulting in longer flakes, with a more usable edge, and with a
more balanced distribution of mass per surface area.

Reduction studies focus on predicting the original mass of scrapers. The
Middle Paleolithic scraper reduction model
\[[26](#ref-dibbleInterpretationMiddlePaleolithic1987a),[27](#ref-dibbleMiddlePaleolithicScraper1995a)\],
for instance, suggests that some stone tools can represent different
stages of reuse and modification through retouch. This has important
implications, as it makes scrapers proxies for understanding the
organization of lithic technology of past groups
\[[23](#ref-andrefskyRawMaterialAvailabilityOrganization1994),[28](#ref-kuhnMousterianLithicTechnology1995),[29](#ref-shottCostsBenefitsTechnological2018)\].
Scrapers can then inform on the management of raw materials, the
selection of morphologies for transport, mobility, site occupation
patters, etc. Central to the reduction model is the concept of
“curation”, defined in lithic studies as “the ratio of realized to
potential utility”
\[[30](#ref-shottExegesisCurationConcept1996),[31](#ref-shottUseLifeCuration2005)\].
This requires estimating what percentage of mass has been consumed by
retouch, thereby making it necessary to estimate the original mass of a
retouched tool. To estimate the curated ratio of a stone tool, lithic
technology has sought an index that meets eight requirements
\[[32](#ref-hiscockGeneralizationInferenceQuantification2010)\]. These
are: inferential power (being capable of accurately estimate the amount
of mass lost); directionality (as reduction proceeds, so too does the
value of the index); comprehensiveness (able to quantify mass lost along
the complete reduction process); sensitivity (detect small differences
in reduction); versatility (not affected by the different retouch
patterns); blank diversity (not affected by the initial morphology of
the flake); and size independence (not affected by the initial size of
flakes).

 

Mechanical flaking experiments have been key for understanding the
specific determinants and their individual effects on flake
morphological variability and size
\[[33](#ref-cotterellEssentialMechanicsConchoidal1985)–[44](#ref-vanpeerMechanicalFrameworkConchoidal2021)\].
Standardized flake production (using homogeneous raw materials,
standardized core morphology and flaking machines) allow control over
individual variables, thereby making is possible to verify and quantify
the individual effect of each variable, and importantly their
interactions, on flake shape, size and additional features
(e.g. platform lipping), along with providing information usually not
available (such as force, hammer morphology, etc.).

 

Research initiated in the 1980s by Harold Dibble and colleagues focused
on exploring flake features measurable in the archaeological lithic
record and controllable by knappers
\[[36](#ref-dibbleEffectHammerMass1995),[37](#ref-dibbleIntroducingNewExperimental2009a),[40](#ref-pelcinControlledExperimentsProduction1996),[41](#ref-pelcinEffectCoreSurface1997a),[43](#ref-pelcinFormationFlakesRole1997),[45](#ref-dibblePlatformVariabilityFlake1997)\].
In the context of controlled experiments, standardized glass cores and a
flaking apparatus were used
\[[36](#ref-dibbleEffectHammerMass1995),[46](#ref-liSynthesisDibbleControlled2023)\],
focusing on and individually modifying the platform depth (PD), external
platform angle (EPA) and angle of blow (AOB). These three variables form
the EPA+PD+AOB model
\[[37](#ref-dibbleIntroducingNewExperimental2009a),[46](#ref-liSynthesisDibbleControlled2023)\],
which in controlled conditions has been demonstrated to determine a
significant portion of flake morphological and size (mass) variability.
When estimating flake mass, the cube root of mass was used as the
response variable. This transformation was applied because mass scales
cubically, whereas key predictors of the model, such as PD, vary at a
linear scale. Expressing mass as its cube root therefore places the
response variable on a scale that is more directly comparable to the
predictors and improves the modelling of their relationship
\[[37](#ref-dibbleIntroducingNewExperimental2009a),[46](#ref-liSynthesisDibbleControlled2023)\].

 

PD is easily measurable in most flakes, and according to this model,
increasing the PD will result in larger but also thicker flakes, with
substantial mass being removed from the core. Increasing EPA, results in
longer flakes, with larger surface areas relative to thickness, thus
with less mass being removed from the core. There is an interaction
between EPA and PD such that at higher EPAs, PD has a larger effect on
flake size. How to measure EPA has been a source of disagreement among
different analysts, with positions ranging from the rejection of this
variable \[[47](#ref-andrefskyLithicsMacroscopicApproaches2005)\], its
use only when reference points are clear
\[[45](#ref-dibblePlatformVariabilityFlake1997)\], or proposals for
alternative ways to measure EPA
\[[48](#ref-cochraneMeasurementAnalysisPlatform2003),[49](#ref-stoutUnderstandingOldowanKnapping2009)\].
However, despite these difficulties, controlled experiments show that
this variable has an important effect on flake shape and size
\[[36](#ref-dibbleEffectHammerMass1995),[37](#ref-dibbleIntroducingNewExperimental2009a),[40](#ref-pelcinControlledExperimentsProduction1996),[46](#ref-liSynthesisDibbleControlled2023)\].
Additionally, hitting the core more obliquely (increasing the AOB)
results in shorter and lighter flakes, and since the flake has a lower
mass, the force required for its detachment diminishes
\[[37](#ref-dibbleIntroducingNewExperimental2009a),[46](#ref-liSynthesisDibbleControlled2023)\].

 

Despite its importance in flake prediction, when applying this model to
the archaeological record AOB is usually dropped from the analysis
because it could not be recorded on flakes. Recent approaches using
controlled experiments and 3D measurements have shown that the AOB can
change the angle of the Hertzian cone at the point of percussion and
thus the angle of the Hertzian cone or bulb angle
\[[50](#ref-liQuantifyingKnappingActions2022)\] can be a proxy for AOB.
The Hertzian cone is not visible on all flakes and even when present it
is small and difficult to measure. One approach is to simply use a
goniometer directly on the flake. Alternatively, the bulb angle can be
measured from 3D models in at least two different ways. One protocol
involves passing a profile through the cone from which landmarks are
extracted to calculate the bulb angle
\[[50](#ref-liQuantifyingKnappingActions2022)\]. Alternatively, there
exists a virtual goniometer plugin for MeshLab which can capture this
angle
\[[50](#ref-liQuantifyingKnappingActions2022)–[52](#ref-cignoniMeshLabOpenSourceMesh2008)\].
Despite their different approaches, both 3D methods have produced
consistent and accurate results, showing strong agreement with one
another and successfully predicting AOB values
\[[50](#ref-liQuantifyingKnappingActions2022)\]. These results
demonstrate that bulb angle measurements can serve as a reliable proxy
for AOB, thereby enabling the estimation of this variable in
archaeological assemblages and incorporating it to models predicting
flake mass \[[53](#ref-liDidEarlyPleistocene2023)\].

 

Thus while either form of the EPA+PD+AOB model is potentially of great
importance in stone tool studies because it focuses on a small group of
variables that can be analyzed in archaeological sites from almost all
periods and regions, the actual application of the model to the
archaeological record has not worked well enough. The main issue is that
flake predictions obtained under controlled conditions are not
replicated under freehand experimental conditions where there is
substantially more variability
\[[54](#ref-bustos-perezMultipleApproachesPredicting2022)–[57](#ref-shottFlakeSizePlatform2000a)\].
Several approaches have been taken to try to improve the model’s
predictive capacity. One approach has been to incorporate new variables,
such as platform surface area \[[57](#ref-shottFlakeSizePlatform2000a)\]
or flake thickness
\[[55](#ref-bustos-perezPredictingFlakeMass2021),[58](#ref-dogandzicEdgeLengthSurface2015)\].
Another approach has been to improve how the key variables are recorded
\[[55](#ref-bustos-perezPredictingFlakeMass2021),[59](#ref-mullerNewMethodAccurately2016)\],
including the use of improved digital measurements
\[[60](#ref-braunLandscapescaleVariationHominin2008),[61](#ref-clarksonEstimatingOriginalFlake2011)\].

 

If, however, we return to the issue of aspects of variability in the
archaeological record not tested in controlled conditions, one key
aspect stands out. In all of Dibble experiments, only a single, primary
flake was extracted from each core and then the core was retired
\[[37](#ref-dibbleIntroducingNewExperimental2009a),[43](#ref-pelcinFormationFlakesRole1997),[46](#ref-liSynthesisDibbleControlled2023)\].
There were practical reasons for this (i.e. the core design did not lend
itself to multiple flakes from the same core), but also Dibble wanted
the core surface morphology to be a constant in most of his experiments.
In only one experiment \[[62](#ref-rezekRelativeEffectsCore2011)\] did
Dibble and colleagues address the impact of core surface morphology on
flake outcomes. They molded glass cores which varied the basic Dibble et
al. core morphology by adding different forms of ridges (parallel,
convergent, center-ridge, divergent). Results from this experiment
\[[62](#ref-rezekRelativeEffectsCore2011)\] concluded that while core
surface morphology does have some effect, PD, EPA and AOB remain the
strongest predictors of flake size and shape. This conclusion ran
somewhat against the common view that the dorsal surface was an
important determinant of flake morphology and mass. For instance, it is
normally assumed that ridges from previous scars can act by guiding the
fracture plane to generate more elongated elements (as observed in Upper
Paleolithic blade productions) or very pronounced concavities from a
previous scar can cause an early exit of fracture plane, resulting in a
shorter or hinged flake. Thus, by excluding flakes with previous scars
from experimental control, an important aspect of actual lithic
variability has gone untested.

 

Thus the effects of the presence of previous flake scars has barely been
explored and can be expected to have several effects on the EPA+PD+AOB
model’s ability to predict flake mass
\[[40](#ref-pelcinControlledExperimentsProduction1996)\]. One possible
scenario is that the model continues to predict well flake mass based on
these three variables. Thus, when faced with a core surface with
multiple flake scars, given a strike with a particular EPA, PD and AOB,
the flake would necessarily come off longer or wider (or perhaps
thicker) in order to achieve the predicted final mass when mass is
missing due to the flake scars. Alternatively, the core surface
morphology, modified by these scars, may act to direct the fracture
plane further along the core surface (similar to perhaps how a blade
core works) producing a flake with greater than predicted mass. In this
scenario the current model would underestimate the mass of flakes with
scars. A third scenario relates to the fact that platform width is not
included in the standard model. In some cases, previous flake removals
along the same platform will mean that the platform is truncated and
this might mean that the initiation of the flake fracture is altered in
a way that impacts the resulting flake size, likely reducing it. These
last two scenarios obviously touch on the EPA+PD+AOB predictive capacity
and could help explain why prediction in archaeological assemblages
preforms less well. Thus here we report on controlled experiments on the
effect of changes to the core surface morphology resulting from previous
flake removals on the ability of the model to predict flake mass.

 

## **2) Methods**

### **2.1) Experimental setting and sample.**

The experimental setup employed here uses the same machine described in
the Dibble et al. mechanical flaking experiments
\[[37](#ref-dibbleIntroducingNewExperimental2009a),[46](#ref-liSynthesisDibbleControlled2023)\].
This device consists of a pneumatic cylinder powered by compressed air
and equipped with a rod assembly bearing a steel hammer tip. When
activated, the rod assembly extends downward and strikes the glass core
positioned below, replicating the “follow-through” motion of a
flintknapper (Figure <a href="#fig:example-experimetns">1</a>: A to C).
The hammer is beveled following the specifications outlined in previous
experimental designs
\[[37](#ref-dibbleIntroducingNewExperimental2009a),[42](#ref-pelcinEffectIndentorType1997),[46](#ref-liSynthesisDibbleControlled2023)\].

 

We used hemispheres made of solid, optically clear K9 glass,
commercially produced by Schreiber Glass. Each hemisphere has two
surfaces: a flat surface corresponding to the equator of the sphere
(serving as the percussion surface) and the curved exterior surface from
which flakes were detached. We used cores with two different diameters:
8 cm and 10 cm. Although industrially manufactured, the cores show some
variability in their initial dimensions and weight. On average, the 8 cm
hemispheres weighed 277.64 ± 9.31 g and measured 3.6 ± 0.1 cm in height,
while the 10 cm hemispheres weighed 561.75 ± 17.37 g and had an average
height of 4.65 ± 0.2 cm. To standardize flake production, four core
holders were designed in FreeCAD 1.0 (<https://www.freecad.org/>) and
3D‑printed: two for the 8 cm cores and two for the 10 cm cores. Each
holder includes a cavity to position and secure the glass hemispheres,
as well as a central front notch that allows flakes to detach freely
(Figure <a href="#fig:example-experimetns">1</a>: D and E). For each
core size, one holder was designed to maintain a horizontal orientation,
providing an AOB of 0°, whereas a second holder was designed to provide
an AOB of 20°. Three C‑clamps and a rectangular metal spacer were used
to evenly press and immobilize the core, with a piece of leather placed
between the metal spacer and the glass to cushion and improve pressure
distribution.

 

``` r
knitr::include_graphics("External-images/knapping_sequence.png")
```

<div class="figure">

<img src="External-images/knapping_sequence.png" alt="Example of some of the mechanical flaking experiments. A to C) Frames from a slow motion video showing the detachment of a secondary flake. D) Preparation and placement of the core prior to the detachment of a secondary flake (note that the tip of the hammer touches the core in order to determine the PD of the flake and then is raised before striking off the flake). E) Example of a mechanically produced secondary flake.  In each of these examples the color comes from paint used to improve the scanning characteristics of the otherwise clear glass." width="740" />
<p class="caption">

<span id="fig:example-experimetns"></span>Figure 1: Example of some of
the mechanical flaking experiments. A to C) Frames from a slow motion
video showing the detachment of a secondary flake. D) Preparation and
placement of the core prior to the detachment of a secondary flake (note
that the tip of the hammer touches the core in order to determine the PD
of the flake and then is raised before striking off the flake). E)
Example of a mechanically produced secondary flake. In each of these
examples the color comes from paint used to improve the scanning
characteristics of the otherwise clear glass.
</p>

</div>

 

The first flaking sequence was aimed at producing flakes without scars
from previous removals (termed “primary flakes” following Sullivan and
Rozen \[[63](#ref-sullivanDebitageAnalysisArchaeological1985)\]). This
was achieved by rotating the cores in the holder and spacing the points
of percussion widely enough to prevent any overlap between removals
(Figure <a href="#fig:example-materials">2</a>). The second sequence
focused on producing flakes that captured scars from previous removals
(termed “secondary flakes” also following Sullivan and Rozen
\[[63](#ref-sullivanDebitageAnalysisArchaeological1985)\]). For these,
the hammer tip was positioned to strike in the space between previous
detachments. As a result, most secondary flakes captured earlier scars
on both lateral margins, while retaining a portion of the original
convex core surface in the center. Importantly too, we made sure that
the platform depth was not impacted by previous removals (though the
platform width could be). Both flaking sequences were conducted using
the same AOB. Thus, if all flakes in the first sequence were produced
with an AOB of 0°, the second sequence was also carried out with an AOB
of 0°, and the same applied for the 20° condition. Failed removals and
flakes fragmented during detachment were excluded from the dataset. Only
complete flakes preserving the morphological and metric attributes
necessary for the analyses, and exhibiting the characteristic features
of conchoidal fracture (ring crack, bulb of percussion, ripple marks,
etc.) were included, ensuring consistent measurement of all variables.
On average, each core yielded between 4 and 5 flakes of each type.

 

``` r
Digit_data2 %>% 
  group_by(Flake_type, coretype, factor(aob)) %>% 
  summarise(
    count = n()
  ) %>% 
  kable(col.names = c("Flake type", "Core size", "AOB", "n"))
```

| Flake type | Core size    | AOB |   n |
|:-----------|:-------------|:----|----:|
| Primary    | HEMISPHERE08 | 0   |  50 |
| Primary    | HEMISPHERE08 | 20  |  48 |
| Primary    | HEMISPHERE10 | 0   |  43 |
| Primary    | HEMISPHERE10 | 20  |  38 |
| Secondary  | HEMISPHERE08 | 0   |  51 |
| Secondary  | HEMISPHERE08 | 20  |  37 |
| Secondary  | HEMISPHERE10 | 0   |  34 |
| Secondary  | HEMISPHERE10 | 20  |  32 |

<span id="tab:flakes-dist"></span>Table 1: Flake type counts according
to core size and AOB. Hemisphere08 and Hemisphere10 refer to the
diameters of the cores.

 

The experimental collection of primary flakes consisted of a sample of
179 flakes (Table <a href="#tab:flakes-dist">1</a>). Flake mass among
this experimental collection varied with an average value of 6.785 g
(cube root value of 1.746), with a maximum value of 36.54 g, while the
smallest flake weighed only 0.21 g. The standard deviation of the cube
root of flake mass was 0.523. The primary flakes included in this study
were previously reported \[[64](#ref-bustos-perezRulesAreRules2026)\].
That assemblage showed a high degree of reproducibility with earlier
experimental studies based on mechanically produced flakes
\[[37](#ref-dibbleIntroducingNewExperimental2009a),[46](#ref-liSynthesisDibbleControlled2023),[62](#ref-rezekRelativeEffectsCore2011),[65](#ref-leaderEffectsPlatformBeveling2017)–[68](#ref-linUtilityEconomizationUnretouched2013)\],
supporting the reliability of the experimental protocol and the
resulting morphological and metric patterns. As the mechanical flaking
experiments continued after the publication of that study as part of the
broader experimental program, the number of primary flakes included here
differs slightly from what we previously reported
\[[64](#ref-bustos-perezRulesAreRules2026)\].

 

``` r
knitr::include_graphics("External-images/Sample-of-flakes.png")
```

<div class="figure">

<img src="External-images/Sample-of-flakes.png" alt="Example of flakes from both experimental collections. Top row: flakes from the Hemisphere dataset obtained during the first round of flaking. Bottom row: flakes capturing scars from previous removals during the second round of detachment." width="764" />
<p class="caption">

<span id="fig:example-materials"></span>Figure 2: Example of flakes from
both experimental collections. Top row: flakes from the Hemisphere
dataset obtained during the first round of flaking. Bottom row: flakes
capturing scars from previous removals during the second round of
detachment.
</p>

</div>

 

The second round of flake production provided an experimental collection
of 154 secondary flakes with scars (Table
<a href="#tab:flakes-dist">1</a>). Flake mass among the secondary flakes
(Table <a href="#tab:desc-stat-flakes">2</a>) varied with an average of
7.188 g (cube root of 1.823). The biggest flake of this experimental
collection weighed 36.78 g, while the smallest flake weighed 0.4 g. The
standard deviation of cube root of flake mass was 0.452 for this
experimental collection (Table <a href="#tab:desc-stat-flakes">2</a>).

 

``` r
rbind(
  (
    Digit_data2 %>% 
      group_by(Flake_type) %>% 
      summarise(
        count =n (),
        min_mass = min(fweight),
        first_q = quantile(fweight, 0.25),
        meadian = quantile(fweight, 0.5),
        mean_mass = mean(fweight),
        fthird_q = quantile(fweight, 0.75),
        max_mass = max(fweight))
  ),
  (
    Digit_data2 %>% 
      group_by(Flake_type) %>% 
      summarise(
        count = n(),
        min_mass = min(mass_cube),
        first_q = quantile(mass_cube, 0.25),
        meadian = quantile(mass_cube, 0.5),
        mean_mass = mean(mass_cube),
        fthird_q = quantile(mass_cube, 0.75),
        max_mass = max(mass_cube)) 
  )) %>% 
  kable(col.names = c("Flake type", "n", "Min", "1st Q", "Median",
                      "Mean", "3rd Q", "Max"), digits = 3)
```

| Flake type |   n |   Min | 1st Q | Median |  Mean | 3rd Q |    Max |
|:-----------|----:|------:|------:|-------:|------:|------:|-------:|
| Primary    | 179 | 0.210 | 2.720 |   5.18 | 6.785 | 9.500 | 36.540 |
| Secondary  | 154 | 0.400 | 4.002 |   6.33 | 7.188 | 8.485 | 36.780 |
| Primary    | 179 | 0.594 | 1.396 |   1.73 | 1.746 | 2.118 |  3.318 |
| Secondary  | 154 | 0.737 | 1.588 |   1.85 | 1.823 | 2.040 |  3.326 |

<span id="tab:desc-stat-flakes"></span>Table 2: Descriptive statistics
of flake mass and cube-root-transformed mass for the experimental
assemblage of mechanically produced flakes.

 

### **2.2) Attribute analysis and 3D landmarking**

All flakes were scanned using an Artec Micro II and exported to PLY
format. Because glass flakes are transparent, structured‑light scanning
cannot reliably capture their geometry. To address this, each flake was
spray‑painted with acrylic paint prior to scanning. This coating
provides long‑term durability, creates an opaque surface suitable for
structured‑light acquisition, and preserves the fine morphological
features characteristic of conchoidal fracture (e.g., eraillure scars,
bulbs, ripples; Figure <a href="#fig:sec-flake-measures">3</a>: A).
Following acquisition, the meshes were decimated to 200,000 faces using
the Rvcg R package v.0.25 \[[69](#ref-schlagerMorphoRvcgShape2017)\].
All scans were visually inspected in Meshlab v.2025.07
\[[52](#ref-cignoniMeshLabOpenSourceMesh2008)\]. A total of 143
landmarks were placed in order to extract accurate measures of flake
features (Bookstein, 1997a, 1997b; Gunz et al., 2005; Gunz and
Mitteroecker, 2013). These landmarks correspond to: three fixed
landmarks (point of percussion and both laterals of the platform); 40
landmarks defining the outline of the platform (10 for the left interior
platform curve, 20 for the exterior platform curve, and 10 for the right
interior platform curve); and 100 landmarks for the flake edge (Figure
<a href="#fig:sec-flake-measures">3</a>: B). All curve‑based landmarks
were positioned at equal intervals along their respective curves.
Landmarking was performed in Viewbox v.4.1.5.2
(<https://www.dhal.com/viewbox.htm>). To guide the placement of
landmarks and semilandmarks, a template/atlas was created using an
arbitrarily selected flake from the Dibble et al. experimental
collection. Prior to extracting measurements from landmarks, all flakes
were rotated without scaling using the package Morpho v.2.12
\[[69](#ref-schlagerMorphoRvcgShape2017)\]. For all flakes, the
following variables were obtained (manual recording was used to maintain
comparability with earlier experiments; Figure
<a href="#fig:sec-flake-measures">3</a>: A):

- Platform depth (PD): measured with a digital caliper (precision of
  0.01 mm) from the point of percussion to the exterior surface of the
  platform.

- Platform width (PW): measured with a digital caliper (precision of
  0.01 mm) from one lateral extremity of the platform edge to the other.

- Platform surface interior angle (PSIA): defined as the angle formed by
  the platform width extremities and the point of percussion
  \[[70](#ref-mcpherronIntroducingPlatformSurface2020)\]. Measured
  digitally by the use of the corresponding landmarks.

- Platform surface area: measured in $mm^2$ on the 3D meshes and defined
  as the area enclosed by the landmarks of the three curves of the
  platform.

- Length: measured with a digital caliper (precision of 0.01 mm) from
  the point of percussion to the most distal point on the flake edge.

- Width: measured with a digital caliper (precision of 0.01 mm) at the
  midpoint of the length axis of the flake and perpendicular to it.

- Thickness: measured with a digital caliper (precision of 0.01 mm) at
  the midpoint of the length axis of the flake.

- Angle of blow (AOB): fixed by the 3D printed core holder with two
  possible values (0º and 20º).

- Fracture angle: the angle in degrees formed between the landmarks
  placed along the flake edge and those defining the platform perimeter
  (Figure <a href="#fig:sec-flake-measures">3</a>: C).

- Mass: measured on a digital scale with a precision of 0.01 g.

 

``` r
knitr::include_graphics("External-images/secondary-flake-measures.png")
```

<div class="figure">

<img src="External-images/secondary-flake-measures.png" alt="3D scan of a secondary flake. A) Example of manual attributes measured from the flakes. B) Landmarking protocol. C) Fitting of the regression vectors to obtain the fracture angle" width="354" />
<p class="caption">

<span id="fig:sec-flake-measures"></span>Figure 3: 3D scan of a
secondary flake. A) Example of manual attributes measured from the
flakes. B) Landmarking protocol. C) Fitting of the regression vectors to
obtain the fracture angle
</p>

</div>

 

### **2.3) Regression analysis and model comparison**

The cubic root of mass is used as the variable to be predicted. This is
done to obtain a linear relationship between PD and mass and to
facilitate comparison with previous results
\[[37](#ref-dibbleIntroducingNewExperimental2009a),[46](#ref-liSynthesisDibbleControlled2023),[58](#ref-dogandzicEdgeLengthSurface2015)\].
PD, AOB and fracture angle serve as predictive variables. For the three
regression models, fracture angle is employed as a substitute of EPA. As
noted previously, how to measure EPA has been a source of disagreement
between analysts
\[[45](#ref-dibblePlatformVariabilityFlake1997),[47](#ref-andrefskyLithicsMacroscopicApproaches2005)–[49](#ref-stoutUnderstandingOldowanKnapping2009)\].
This difficulty arises because flakes may exhibit a curved cortical
dorsal surface or, in the absence of cortex, an irregular topography
generated by previous flake scars or platform beveling. In addition, the
platform itself may present surface irregularities. Together, these
factors can obscure the reference points required for EPA measurement,
increasing inter-observer variability and reducing measurement
reliability. The experimental cores employed in this study have a
consistently curved surface, which results in continuous variation
across the dorsal surface of flakes. This design intentionally simulates
situations in which EPA cannot be measured with confidence. Using
fracture angle in place of EPA therefore allows the models to evaluate
flake formation under conditions that mirror many archaeological
contexts where EPA is obscured or distorted.

 

Two models are trained to estimate the cube root of flake mass: one
using flakes without previous scars (primary flakes), and a second model
using flakes with scars (secondary flakes). All models were trained
using a multiple linear regression and evaluated through the leave one
out cross validation system. The multiple linear regression extends the
linear regression in such a way that it can directly accommodate
multiple predictors
\[[71](#ref-jamesIntroductionStatisticalLearning2013)\]. In the leave
one out cross validation system, each row of a data set serves as a test
set, while the rest serve as a training set. As a result of this method,
the same number of models as the sample size are trained and the
accuracy metrics are extracted from each of the rows that served as test
sets. This approach reduces the bias of the model and ensures model
results.

 

From these two models, three types of predictions are obtained. The
first group of predictions comes from the model trained on the primary
flakes predicting those same flakes. This model is equivalent to those
developed in previous experiments
\[[37](#ref-dibbleIntroducingNewExperimental2009a),[46](#ref-liSynthesisDibbleControlled2023)\]
and serves as a baseline reference for models trained using primary
flakes (without previous scars). The second group of predictions comes
from applying the primary flake model to predict the flake mass (cube
root) of secondary flakes. This approach allows us to evaluate whether
the predictive performance of models trained on primary flakes can be
reliably extended to secondary flakes and to assess whether any
systematic bias emerges in these predictions. The third group of
predictions comes from a model trained on the secondary flakes and used
to predict the mass (cube root) of the same secondary flakes. This
provides a direct comparison with the previous two models (and their
predictions) and allows us to determine whether the presence of scars
affects the ability of the selected variables (PD, AOB and fracture
angle) to predict flake mass.

 

The accuracy of the predictions of the three groups was compared using
four metrics: $r^2$, MAE, RMSE and MAPE. $r^2$ quantifies the strength
of the linear correlation and indicates the proportion of observed
variation accounted for by the model. The addition of predictors results
in increasing $r^2$ values, irrespective of predictor contribution to
the model and making it impossible to compare models with a different
number of predictors. The adjusted $r^2$ is analogous to the linear
regression, but adjusted to the number of predictors, making possible
comparisons of models with different numbers of predictors. All $r^2$
values reported here correspond to the adjusted $r^2$. In lithic
studies, $r^2$ values have been used to classify the predictive power of
indices \[[32](#ref-hiscockGeneralizationInferenceQuantification2010)\],
with values below 0.1 considered low, 0.1–0.25 moderate, 0.26–0.5 fairly
large or strong, 0.51–0.8 very large or strong, and values above 0.8
regarded as extremely large or strong. Nevertheless, it should be noted
that data sets with differing distributions may yield identical or
similar $r^2$ values
\[[72](#ref-anscombeGraphsStatisticalAnalysis1973)\]. Because of this,
multiple studies focused on predicting original flake mass favor other
measures of precision
\[[54](#ref-bustos-perezMultipleApproachesPredicting2022),[55](#ref-bustos-perezPredictingFlakeMass2021),[73](#ref-bustos-perezFindingOriginalMass2025)\].

 

The mean average error (MAE) and root mean squared error (RMSE) provide
summary values of how far predictions fall from the true value
\[[71](#ref-jamesIntroductionStatisticalLearning2013),[74](#ref-lantzMachineLearningExpert2019)\].
The mean absolute error (MAE) measures the average magnitude of
prediction errors, regardless of their direction. The root mean square
error (RMSE) similarly quantifies the deviation between predicted and
observed values but assigns greater weight to larger errors. RMSE is
often evaluated relative to the standard deviation (SD) of the predicted
variable. When the RMSE is lower than the SD, the model performs better
than a simple mean‑based prediction. An ideal model would produce MAE
and RMSE values of zero, and in practice, lower values for both metrics
indicate better predictive performance. The mean absolute percentage
error (MAPE) quantifies prediction error relative to the observed values
by expressing it as a percentage
\[[75](#ref-demyttenaereMeanAbsolutePercentage2016)\]. It indicates the
average percentage deviation of the predicted values from the
corresponding actual values, facilitating the interpretation of model
accuracy in relative terms.

 

A regression plot displays the relationship between predicted and
observed values, along with the fitted regression line. In a
well‑performing model, this line should pass through the center of all
points, which should be distributed evenly above and below it. The plot
also includes an identity line, representing perfect agreement between
observed and predicted values. Comparing the regression line to this
identity line makes it possible to detect systematic biases across the
prediction range.

 

The analysis of residuals is essential for identifying potential biases
in model predictions. To this end, a visual exploratory assessment of
the residuals was conducted. First, the overall density distribution of
the residuals was examined. In an unbiased model, residuals should peak
around the zero value and display a roughly symmetrical distribution on
both sides. Residual plots were then used to compare residuals with the
predicted values of cube root of flake mass. Examining residuals in
relation to predicted mass allows us to identify the direction of any
bias. For example, if residual values decrease with predicted flake
mass, this would indicate that the model systematically overestimates
the mass of larger flakes. In an ideal plot, residuals should be evenly
distributed along the 0 axis. Additional to the residual plots,
residuals were regressed on predicted values. If the estimate of the
predicted value for this regression is not equal to zero, this indicates
that the model has systematic bias across the prediction range
\[[71](#ref-jamesIntroductionStatisticalLearning2013)\].

 

In the EPA+PD+AOB model, PD has the largest effect size, and controlled
experiments typically show a strong correlation between PD and mass. For
this reason, residuals were also examined as a function of PD to
determine whether prediction errors vary systematically with changes in
PD. As with the predicted values, the estimate of PD when predicting
residuals should be equal or close to zero. Otherwise, this indicates
that the model has systematic bias across the range of PD values. This
allows us to determine whether there are error biases in the prediction
as PD values change. The residual plots were generated for all three
prediction groups (primary‑to‑primary; primary‑to‑secondary; and
secondary-to-secondary).

Previous research has shown that initial core size, together with PD and
AOB, can influence the amount of mass detached by a flake
\[[64](#ref-bustos-perezRulesAreRules2026)\]. To evaluate whether this
variable affected model performance, residual distributions were
examined according to core size through the three groups of predictions.

 

In addition to evaluating model performance, the effect of previous
scars on flake linear measurements was examined. To do this, linear
measurements were analysed in relation to platform depth and AOB. For
each measurement, multiple linear regressions were then fitted using the
same predictors employed for flake mass, with the addition of flake
type. This approach makes it possible to assess the specific
contribution of flake type and to evaluate how the presence of dorsal
scars alters the coefficient values, thereby identifying which linear
dimensions are most affected by previous removals.

 

The complete analysis workflow was performed using the R programming
language v.4.4.1 in RStudio IDE v.2025.05.01
\[[76](#ref-rcoreteamLanguageEnvironmentStatistical2019),[77](#ref-rstudioteamRStudioIntegratedDevelopment2019)\].
Data processing, transformation and plotting was done using the
*tidyverse* package v.2.0.0 \[[78](#ref-wickhamWelcomeTidyverse2019)\].
Model training, and predictions were done using the *caret* v.7.0.1
package \[[79](#ref-kuhnBuildingPredictiveModels2008)\].

 

## **3) Results**

### **3.1) Model evaluation and bias**

Figure <a href="#fig:reg-plots">4</a> presents the regression plots for
the three groups of predictions (primary-to-primary;
primary-to-secondary; secondary-to-secondary). All three groups of
predictions seem to present a good fit, with the points distributed
evenly on both sides of their respective regression lines.

 

Comparing the regression line with the identity line across the
different prediction groups reveals model‑specific differences in
calibration (Figure <a href="#fig:reg-plots">4</a>). For the model
trained on primary flakes and used to predict primary flakes, the
regression and identity lines lie very close to one another. At the
lower end of the flake‑mass range, the regression line sits slightly
above the identity line, indicating minor overpredictions, whereas at
higher masses it falls slightly below the identity line, indicating
small underpredictions. When the model trained on primary flakes was
applied to secondary flakes, the regression and identity lines are the
closest of the three models. This suggests that the model maintains good
calibration across the full range of flake‑mass values, even when
predicting flakes with scars. The model trained and tested on secondary
flakes showed the greatest deviation between the regression and identity
lines. This pattern indicates that this model tends to overestimate the
mass of smaller flakes and underestimate the mass of larger flakes,
reflecting the strongest systematic bias among the three prediction
groups.

 

``` r
ggpubr::ggarrange(
  (
    as.data.frame(MLR_Prim$pred) %>% 
  ggplot(aes(obs, pred)) +
  geom_point(fill = "darkgreen", shape = 21) +
  coord_fixed() +
  theme_light() +
  labs(title = "Trained and predicting on primary flakes",
       x = "Observed flake mass (cube root)",
       y = "Predicted mass (cube root)") + 
  scale_x_continuous(breaks = seq(0, 3, 0.2), limits = c(0.8,2.9)) +
  scale_y_continuous(breaks = seq(0, 3, 0.2), limits = c(0.8,2.9)) +
  geom_smooth(method='lm', se = FALSE) +
  geom_abline(linetype = "dashed") +
  
  theme(
    axis.text = element_text(size = 7, color = "black"),
    axis.title = element_text(size = 8, color = "black", face = "bold"),
    plot.title = element_text(size = 7, color = "black", face = "bold"))
  ),
  
  (Sec_Flakes %>% ggplot(aes(mass_cube, pred_from_prim)) +
     geom_point(fill = "#007BA7", shape = 21) +
     coord_fixed() +
     theme_light() +
     labs(title = "Trained on primary flakes, predicting on secondary flakes",
          x = "Observed flake mass (cube root)",
          y = "Predicted mass (cube root)") + 
     scale_x_continuous(breaks = seq(0, 3, 0.2), limits = c(0.8,2.9)) +
     scale_y_continuous(breaks = seq(0, 3, 0.2), limits = c(0.8,2.9)) +
     geom_smooth(method='lm', se = FALSE) +
     geom_abline(linetype = "dashed") +

     theme(
       axis.text = element_text(size = 7, color = "black"),
       axis.title = element_text(size = 8, color = "black", face = "bold"),
       plot.title = element_text(size = 7, color = "black", face = "bold")
     )
  ),
  
  (
    Sec_Flakes %>% ggplot(aes(mass_cube, pred_from_sec)) +
      geom_point(fill = "#CD5C5C", shape = 23) +
      coord_fixed() +
      theme_light() +
      labs(title = "Trained and predicting on secondary flakes",
           x = "Observed flake mass (cube root)",
           y = "Predicted mass (cube root)") + 
      scale_x_continuous(breaks = seq(0, 3, 0.2), limits = c(0.8,2.9)) +
      scale_y_continuous(breaks = seq(0, 3, 0.2), limits = c(0.8,2.9)) +
      geom_smooth(method = "lm", se = FALSE) +
      geom_abline(linetype = "dashed") +
      theme(
        axis.text = element_text(size = 7, color = "black"),
        axis.title = element_text(size = 8, color = "black", face = "bold"),
        plot.title = element_text(size = 7, color = "black", face = "bold")
      )
  )
)
```

<div class="figure">

<img src="v02-Scars-PD-and-AOB_files/figure-gfm/reg-plots-1.png" alt="Regression plots when estimating flake mass from the either primary flakes or from secondary flakes."  />
<p class="caption">

<span id="fig:reg-plots"></span>Figure 4: Regression plots when
estimating flake mass from the either primary flakes or from secondary
flakes.
</p>

</div>

 

Analysis of the model coefficients provides insight into how each
variable contributes to the amount of mass removed by a flake. All three
variables were significant for estimating flake mass in both models. In
the model trained on primary flakes, PD had an estimated coefficient of
0.22, whereas in the model trained on secondary flakes the PD estimate
was 0.16. This indicates that, for the same PD value, secondary flakes
will have less mass than primary flakes. The AOB shows a negative
relationship in both models, with more oblique detachments (higher AOB
values) producing flakes with less mass. However, the magnitude of this
effect differs between models. In the primary‑flake model, flakes
detached at an AOB of 20° have a cube‑root mass that is 0.26 units lower
than flakes detached at 0°. In the secondary‑flake model, the AOB
coefficient is roughly half as large (0.13), indicating a weaker effect.
This suggests that although oblique detachments also reduce the mass of
secondary flakes, the reduction is less pronounced (likely because
secondary flakes already contain less available mass due to scars from
previous removals). Both models produced nearly identical coefficient
estimates for fracture angle (-0.02). In the hemisphere cores, shorter
flakes tend to have higher fracture‑angle values because the fracture
exits the core earlier, whereas longer flakes penetrate deeper into the
core, increasing flake curvature and thereby reducing the fracture angle
value. This relationship is consistent across both primary and secondary
flakes.

 

Table <a href="#tab:model-results">3</a> presents the percentage of
variance explained by each group of predictions and their precision
metrics. The baseline model (primary-to-primary) explained the highest
percentage of variance, presenting a extremely strong relationship with
the outcome. The RMSE (0.214) of this model was more than 2.5 times
lower than the standard deviation of the outcome 0.523, indicating a
very good fit and that the predictions are not far from the actual
value.

 

However, when this model is applied to predict the flake mass (cube
root) of the secondary flakes, all evaluation metrics worsen. The $r^2$
explained the smallest percentage of variance of the three groups of
predictions (0.7), although it remained strong. The RMSE and MAE
presented the highest values of the three groups of predictions (0.224
and 0.278 respectively), and average percent error (MAPE) regarding
observed flake mass was also the highest. This indicates the largest
magnitude of errors among the three groups of predictions. When the RMSE
is compared to the standard deviation of the cube root of the mass of
the secondary flakes (0.45), it is only 2.02 times smaller, marking a
substantial difference regarding the predictions on primary flakes.

 

The secondary-to-secondary group of predictions presented mixed results.
On the one hand, the percentage of variance explained is similar to the
one obtained when using the primary flakes to predict the mass of the
secondary flakes 0.705. However, the accuracy metrics of predictions
(RMSE, MAE and MAPE) fall in between the other two groups of predictions
(0.193; 0.245 respectively).

 

``` r
tibble(
  Model_to_data = c("Primary to primary", "Primary to secondary", "Secondary to secondary"),
  r2 = c(round(MLR_Prim$results[[3]], 3),
         round(summary(lm((Sec_Flakes$mass_cube ~ Sec_Flakes$pred_from_prim)))$r.squared, 3),
         round(summary(lm((Sec_Flakes$mass_cube ~ Sec_Flakes$pred_from_sec)))$r.squared, 3)),
  
  MAE = c(round(MLR_Prim$results[[4]], 3), 
          round(caret::MAE(Sec_Flakes$mass_cube, Sec_Flakes$pred_from_prim),3),
          round(caret::MAE(Sec_Flakes$mass_cube, Sec_Flakes$pred_from_sec), 3)),
  
  RMSE = c(round(MLR_Prim$results[[2]], 3), 
           round(caret::RMSE(Sec_Flakes$mass_cube, Sec_Flakes$pred_from_prim),3),
           round(caret::RMSE(Sec_Flakes$mass_cube, Sec_Flakes$pred_from_sec), 3)),
    MAPE = c(
    Metrics::mape(MLR_Prim$pred$obs, MLR_Prim$pred$pred)*100,
    Metrics::mape(Sec_Flakes$mass_cube, Sec_Flakes$pred_from_prim)*100,
    Metrics::mape(Sec_Flakes$mass_cube, Sec_Flakes$pred_from_sec)*100
  )) %>% kable(col.names = c("Trained on, to predict", "r2", "MAE", "RMSE", "MAPE"), digits = 3)
```

| Trained on, to predict |    r2 |   MAE |  RMSE |   MAPE |
|:-----------------------|------:|------:|------:|-------:|
| Primary to primary     | 0.831 | 0.175 | 0.214 | 11.329 |
| Primary to secondary   | 0.700 | 0.224 | 0.278 | 12.906 |
| Secondary to secondary | 0.705 | 0.193 | 0.245 | 11.784 |

<span id="tab:model-results"></span>Table 3: Variance explained and
precision metrics for each of the models

 

The average value of the residuals is very similar, with values close to
0 in all three cases. However, visual exploration of the residuals
through density curves (Figure <a href="#fig:dens-plots">5</a>) shows
significant differences in how the residuals are distributed across the
three models. The density curve of the reference model shows a clear
peak close to the zero, although slightly shifted toward negative values
(indicating a tendency to overestimate the flake mass). 50% of the
residuals from the reference model are distributed between -0.16 and
0.16, which is the narrowest distribution range of the three models.

 

The density curve of the residuals obtained from the
primary-to-secondary model showed the least pronounced peak of the three
groups of predictions, although the residuals are normally distributed
(W =-0.17; *p* = 0.17). The peak of this density curve is also slightly
shifted toward negative values, indicating a tendency to overestimate
the mass of the secondary flakes when making predictions. As a result of
this distribution, the residuals of these predictions have the widest
range, with 50% of them falling between the -0.17 and 0.17 values.

 

The density curve of the residuals obtained from the model trained using
the secondary flakes to predict on those same flakes presents a good
distribution. This density curve is symmetrically distributed around the
0 value, although it presents a very wide profile. 50% of the residuals
for this model are comprised between the -0.16 and 0.17, indicating a
slightly better fit than the model trained on the primary flakes and
predicting on the secondary flakes. Residuals from this model are also
normally distributed (W = 0.99; *p* = 0.79).

 

``` r
Res_Allmodels %>% 
  ggplot() +
  geom_density(aes(Residuals, color = Model)) +
  theme_light() +
  scale_color_manual(values = c("darkgreen", "#007BA7", "#CD5C5C"),
                     labels = c("Primary to primary", "Primary to secondary", "Secondary to secondary")) +
  ylab("Density") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "solid") +
  theme(
    legend.position = "bottom",
    axis.text = element_text(size = 7, color = "black"),
    axis.title = element_text(size = 8, color = "black", face = "bold"),
    legend.title = element_text(size = 8.5, color = "black"),
    legend.text = element_text(size = 8, color = "black")
  )
```

<div class="figure">

<img src="v02-Scars-PD-and-AOB_files/figure-gfm/dens-plots-1.png" alt="Scaled density plots of residual distribution among the three models"  />
<p class="caption">

<span id="fig:dens-plots"></span>Figure 5: Scaled density plots of
residual distribution among the three models
</p>

</div>

 

Figure <a href="#fig:res-plots">6</a> presents the distribution of
residuals according to the predicted value of mass (cube root) and PD.
In the case of residuals according to the predicted value, differences
in the distribution of the residuals are observed depending on the
model. In the case of the model trained using primary flakes, the
residuals are distributed symmetrically around the value 0. When the
residuals of the primary-to-primary flakes are regressed on the
predicted values, an estimate of 0 is obtained. This indicates that
there is no bias along the range of predictions for the baseline model.
The primary-to-secondary predictions present a much different
distribution. When flakes are predicted to have a cube root mass above
the value of 2, negative residuals are almost systematically obtained.
When the primary-to-secondary residuals are regressed on the predicted
values, an estimate value of -0.25 is obtained. This is indicative of a
systematic bias along the predicted range, indicating that the larger
the flake, the more its mass gets overestimated. The
secondary-on-secondary residuals are symmetrically distributed around
the value 0. The regression of residuals on predicted values resulted on
a coefficient estimate of 0, which indicates no systematic bias along
the range of predictions.

 

``` r
ggpubr::ggarrange(
  
  # First row title
  ggplot() +
    theme_void() +
    labs(title = "Trained and predicting on primary flakes") +
    theme(
      plot.title = element_text(
        hjust = 0.5, face = "bold", size = 8
      ),
      plot.margin = margin(0, 0, 0, 0)
    ),
  
  # First row plots
  ggpubr::ggarrange(
    as.data.frame(MLR_Prim$pred) %>% 
      mutate(residual = obs - pred) %>% 
      ggplot(aes(pred, residual)) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      geom_point(fill = "darkgreen", shape = 21) +
      theme_light() +
      labs(
        x = "Predicted mass (cube root)",
        y = "Residuals"
      ) +
      scale_y_continuous(breaks = seq(-0.5, 0.5, 0.1), limits = c(-0.5, 0.52)) +
      scale_x_continuous(breaks = seq(0, 2.6, 0.2), limits = c(0.9, 2.6)) +
      theme(
        axis.text = element_text(size = 7, color = "black"),
        axis.title = element_text(size = 8, color = "black", face = "bold")
      ),
    
    Prim_Flakes %>% 
      mutate(res = MLR_Prim$pred$obs - MLR_Prim$pred$pred) %>% 
      ggplot(aes(platthick, res)) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      geom_point(fill = "darkgreen", shape = 21) +
      theme_light() +
      ylab("Residuals") +
      scale_y_continuous(breaks = seq(-0.5, 0.5, 0.1), limits = c(-0.5, 0.52)) +
      scale_x_continuous(breaks = seq(0, 6.6, 1), limits = c(0.4, 7)) +
      xlab("Flake PD (mm)") +
      theme(
        axis.text = element_text(size = 7, color = "black"),
        axis.title = element_text(size = 8, color = "black", face = "bold")
      ),
    ncol = 2
  ),
  
  # Second row title
  ggplot() +
    theme_void() +
    labs(title = "Trained on primary flakes, predicting on secondary flakes") +
    theme(
      plot.title = element_text(
        hjust = 0.5, face = "bold", size = 8
      ),
      plot.margin = margin(0, 0, 0, 0)
    ),
  
  # Second row plots
  ggpubr::ggarrange(
    Sec_Flakes %>% 
      ggplot(aes(pred_from_prim, Res_Prim)) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      geom_point(fill = "#007BA7", shape = 21) +
      theme_light() +
      ylab("Residuals") +
      xlab("Predicted mass (cube root)") +
      scale_y_continuous(breaks = seq(-0.5, 0.5, 0.1), limits = c(-0.5, 0.52)) +
      scale_x_continuous(breaks = seq(0, 2.6, 0.2), limits = c(0.9, 2.6)) +
      theme(
        axis.text = element_text(size = 7, color = "black"),
        axis.title = element_text(size = 8, color = "black", face = "bold")
      ),
    
    Sec_Flakes %>% 
      ggplot(aes(platthick, Res_Prim)) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      geom_point(fill = "#007BA7", shape = 21) +
      theme_light() +
      ylab("Residuals") +
      scale_y_continuous(breaks = seq(-0.5, 0.5, 0.1), limits = c(-0.5, 0.52)) +
      scale_x_continuous(breaks = seq(0, 6.6, 1), limits = c(0.4, 7)) +
      xlab("Flake PD (mm)") +
      theme(
        axis.text = element_text(size = 7, color = "black"),
        axis.title = element_text(size = 8, color = "black", face = "bold")
      ),
    ncol = 2
  ),
  
  # Third row title
  ggplot() +
    theme_void() +
    labs(title = "Trained and predicting on secondary flakes") +
    theme(
      plot.title = element_text(
        hjust = 0.5, face = "bold", size = 8
      ),
      plot.margin = margin(0, 0, 0, 0)
    ),
  
  # Third row plots
  ggpubr::ggarrange(
    Sec_Flakes %>% 
      ggplot(aes(pred_from_sec, Res_Sec)) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      geom_point(fill = "#CD5C5C", shape = 23) +
      theme_light() +
      ylab("Residuals") +
      scale_y_continuous(breaks = seq(-0.5, 0.5, 0.1), limits = c(-0.5, 0.52)) +
      scale_x_continuous(breaks = seq(0, 2.6, 0.2), limits = c(0.9, 2.6)) +
      xlab("Predicted mass (cube root)") +
      theme(
        axis.text = element_text(size = 7, color = "black"),
        axis.title = element_text(size = 8, color = "black", face = "bold")
      ),
    
    Sec_Flakes %>% 
      ggplot(aes(platthick, Res_Sec)) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      geom_point(fill = "#CD5C5C", shape = 23) +
      theme_light() +
      ylab("Residuals") +
      scale_y_continuous(breaks = seq(-0.5, 0.5, 0.1), limits = c(-0.5, 0.52)) +
      scale_x_continuous(breaks = seq(0, 6.6, 1), limits = c(0.4, 7)) +
      xlab("Flake PD (mm)") +
      theme(
        axis.text = element_text(size = 7, color = "black"),
        axis.title = element_text(size = 8, color = "black", face = "bold")
      ),
    ncol = 2
  ),
  
  ncol = 1,
  heights = c(0.12, 1, 0.12, 1, 0.12, 1)
)
```

<div class="figure">

<img src="v02-Scars-PD-and-AOB_files/figure-gfm/res-plots-1.png" alt="Residual distribution according to predicted flake mass and flake PD for the three groups of predictions"  />
<p class="caption">

<span id="fig:res-plots"></span>Figure 6: Residual distribution
according to predicted flake mass and flake PD for the three groups of
predictions
</p>

</div>

 

When residuals are examined according to PD (Figure
<a href="#fig:res-plots">6</a>), a clear bias emerges in the
primary‑to‑secondary predictions. For the primary‑to‑primary and
secondary‑to‑secondary models, residuals remain evenly distributed
around the zero value across the full range of PD values, and the
regression of residuals on PD yields coefficient estimates of 0,
indicating no systematic bias. In contrast, the primary‑to‑secondary
predictions show a pronounced trend: for flakes with a PD above 4.5 mm,
the model consistently overestimates flake mass, producing negative
residuals. Regressing the residuals of this group on PD values results
in a coefficient estimate of -0.07, confirming the presence of
PD‑related bias in this prediction scenario.

 

Analysis of residual according to initial core size allows to evaluate
further bias among the group of predictions. For the primary-to-primary
model, residuals exhibited a clear systematic bias. Flakes detached from
8 cm diameter cores yielded a mean residual of -0.12, indicating a
slight overestimation of cube-root transformed flake mass. In contrast,
flakes detached from 10 cm diameter cores yielded a mean residual 0.16,
indicating a slight underestimation. An ANOVA performed on residuals
grouped by core size confirmed that these differences were statistically
significant (*F* = 81.18; df = 1; *p* \< 0.001).

The same pattern was observed in the primary-to-secondary and
secondary-to-secondary prediction groups. In both cases, predictions for
flakes detached from 8 cm diameter cores systematically overestimated
cube-root transformed flake mass, with mean residuals of -0.13 and -0.11
respectively. Conversely, predictions for flakes detached from 10 cm
diameter cores systematically underestimated flake mass, yielding
positive mean residuals of 0.19 in both groups. ANOVA results further
demonstrated that residual distributions differed significantly
according to core size for both the primary-to-secondary (*F* = 88.56;
df = 1; *p* \< 0.001) and secondary-to-secondary ( *F* = 89.47; df = 1;
*p* \< 0.001) prediction groups.

Overall, model residuals consistently varied with initial core size
across all prediction groups, with a tendency to overestimate the mass
of flakes detached from smaller cores and underestimate the mass of
flakes detached from larger cores.

 

### **3.2) Fracture initiation and propagation according to flake type**

Variables associated with fracture initiation showed limited differences
between primary and secondary flakes. Flake type was not a significant
predictor of either platform width (*p* = 0.74) or PSIA (*p* = 0.24).
Platform width was strongly determined by PD (*p* \< 0.01; $r^2$ =
0.83), increasing by 4.03 mm for each 1 mm increase in PD, whereas PSIA
averaged 137.71º ± 10.4 across the complete sample. Platform surface
area was the only fracture-initiation variable significantly affected by
flake type (*p* = 0.01). Nevertheless, most of its variation was
explained by PD ($r^2$ = 0.93), with the square root of platform surface
area increasing by 1.53 units for each 1 mm increase in PD. After
controlling for PD, secondary flakes were estimated to have
square-root-transformed platform surface areas that were, on average,
0.19 units greater than those of primary flakes.

 

Figure <a href="#fig:measures-flakes">7</a> shows the relationship
between the linear measurements of the flakes (length, width, and
thickness) and the variables PD and AOB, separated by flake type. When
the same predictors used for estimating flake mass are applied to linear
regression models of these measurements, clear patterns emerge according
to flake type.

For flake length, the model explains a substantial proportion of the
variance ($r^2$ = 0.68), and flake type is not a significant factor
($r^2$ = 0.05). In contrast, flake type is significant for both width
and thickness, although it affects each measurement differently. Width
shows the lowest proportion of variance explained among the three models
($r^2$ = 0.58). Flake type strongly influences width, with secondary
flakes being on average 7.17 mm narrower than primary flakes. This
pattern is visible for both AOB values, although it is most pronounced
when flakes are detached perpendicularly to the core surface (AOB of
0º). Thickness is the best predicted of the three linear measurements
($r^2$ = 0.83). For this variable, flake type shows a positive
relationship, with secondary flakes being on average 0.58 mm thicker
than primary flakes. Together, these results indicate that while flake
type has little effect on length, it plays a substantial role in shaping
width and thickness, and these effects vary systematically across the
experimental conditions.

 

``` r
ggpubr::ggarrange(
  (
    Digit_data2 %>% 
      pivot_longer(
        cols = c(length, width, thick),
        values_to = "values",
        names_to = "Metrick_measures"
      ) %>% 
      mutate(Metrick_measures = factor(Metrick_measures,
                                       levels = c("length", "width", "thick"),
                                       labels = c("Length (mm)", "Width (mm)", "Thickness (mm)"))) %>% 
      filter(aob == 0) %>% 
      
      ggplot(aes(platthick, values, fill = Flake_type, shape = Flake_type)) +
      geom_point() +
      stat_smooth(aes(color = Flake_type), method = "lm", 
                  se = FALSE, show.legend = FALSE) +
      labs(title = "AOB 0º",
           x = "Platform depth (mm)",
           y = "Linear mesurement (mm)") + 
      facet_wrap(~ Metrick_measures, scales = "free_y") +
      theme_light() +
      scale_shape_manual(values = c(21, 23)) +
      scale_fill_manual(values = c("#007BA7", "#CD5C5C")) +
      scale_color_manual(values = c("#007BA7", "#CD5C5C")) +
      labs(fill = "Flake type", shape = "Flake type") +
      theme(
        title = element_text(size = 10, color = "black"),
        axis.text = element_text(size = 7, color = "black"),
        axis.title = element_text(size = 8, color = "black", face = "bold"),
        plot.title = element_text(size = 7, color = "black", face = "bold"),
        strip.text = element_text(color = "black", face = "bold", size = 8),
        strip.background = element_rect(fill = "white", colour = "black", linewidth = 1)
      )
  ),
  
  (
    Digit_data2 %>% 
      pivot_longer(
        cols = c(length, width, thick),
        values_to = "values",
        names_to = "Metrick_measures"
      ) %>% 
      mutate(Metrick_measures = factor(Metrick_measures,
                                       levels = c("length", "width", "thick"),
                                       labels = c("Length (mm)", "Width (mm)", "Thickness (mm)"))) %>% 
      filter(aob == 20) %>% 
      
      ggplot(aes(platthick, values, fill = Flake_type, shape = Flake_type)) +
      geom_point() +
      stat_smooth(aes(color = Flake_type), method = "lm", 
                  se = FALSE, show.legend = FALSE) +
      labs(title = "AOB 20º",
           x = "Platform depth (mm)",
           y = "Linear mesurement (mm)") + 
      facet_wrap(~ Metrick_measures, scales = "free_y") +
      theme_light() +
      scale_shape_manual(values = c(21, 23)) +
      scale_fill_manual(values = c("#007BA7", "#CD5C5C")) +
      scale_color_manual(values = c("#007BA7", "#CD5C5C")) +
      labs(fill = "Flake type", shape = "Flake type") +
      theme(
        title = element_text(size = 10, color = "black"),
        axis.text = element_text(size = 7, color = "black"),
        axis.title = element_text(size = 8, color = "black", face = "bold"),
        plot.title = element_text(size = 7, color = "black", face = "bold"),
        strip.text = element_text(color = "black", face = "bold", size = 8),
        strip.background = element_rect(fill = "white", colour = "black", linewidth = 1)
      )
  ),
  
  common.legend = TRUE,
  ncol = 1,
  legend = "bottom"
  
) 
```

<div class="figure">

<img src="v02-Scars-PD-and-AOB_files/figure-gfm/measures-flakes-1.png" alt="Linear measurements according to flake type and AOB"  />
<p class="caption">

<span id="fig:measures-flakes"></span>Figure 7: Linear measurements
according to flake type and AOB
</p>

</div>

 

The remaining predictive variables show well‑defined relationships with
the linear measurements. PD is the most significant variable and has the
strongest effect across all three measurements, presenting a positive
relationship in every case. For both flake types, each millimeter
increase in PD results on average in an increase of 2.67 mm in flake
length, 2.79 mm in flake width, and 0.75 mm in flake thickness. AOB is
also significant for all three measurements, although increasing AOB
produces negative relationships. Flakes detached at an AOB of 20º are,
on average, 0.08 mm shorter, 0.17 mm narrower, and 0.03 mm thinner than
those detached perpendicularly. Because shorter flakes tend to exhibit
higher fracture angles, length decreases by 0.48. Fracture angle is not
significant for predicting flake width (*p* = 0.93) or thickness.
Fracture angle is significant for predicting flake thickness, but its
effect is minimal: each degree increase in fracture angle results in
flakes that are on average 0.07 mm thinner.

 

Figure <a href="#fig:fa-per-flake-type">8</a> summarizes the
relationship between fracture angle, PD, and AOB for both flake types.
Fracture angle is moderately well predicted ($r^2$ = 0.57). At a general
level, flake type influences fracture angle, with secondary flakes
showing values that are on average 2.07º lower than those of primary
flakes when PD and AOB are held constant. However, this effect is not
uniform across AOB values. For flakes detached with an AOB perpendicular
to the core surface, flake type is not a significant predictor of
fracture angle (*p* = 0.26). In contrast, flake type becomes highly
significant for flakes detached with an AOB of 20º (*p* \< 0.001), where
secondary flakes exhibit fracture angles that are on average 3.04º lower
than primary flakes. These results indicate that although some
differences exist, fracture angle behaves in a broadly similar manner
across both flake types, with the influence of flake type emerging
primarily under more oblique striking conditions.

 

``` r
Digit_data2 %>%
  ggplot(aes(platthick, frac_angle, fill = Flake_type, shape = Flake_type)) +
  geom_point() +
  stat_smooth(aes(color = Flake_type), method = "lm", 
              se = FALSE, show.legend = FALSE) +
  labs(
       x = "Platform Depth (mm)",
       y = "Fracture angle (º)") + 
  facet_wrap(~ factor(aob), scales = "free",
             labeller = as_labeller(c("0" = "AOB 0º",
                                     "20" = "AOB 20º"))) +
  theme_light() +
  scale_shape_manual(values = c(21, 23)) +
  scale_fill_manual(values = c("#007BA7", "#CD5C5C")) +
  scale_color_manual(values = c("#007BA7", "#CD5C5C")) +
  labs(fill = "Flake type", shape = "Flake type") +
  
  theme(
    legend.position = "bottom",
    title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 7, color = "black"),
    axis.title = element_text(size = 8, color = "black", face = "bold"),
    plot.title = element_text(size = 7, color = "black", face = "bold"),
    strip.text = element_text(color = "black", face = "bold", size = 8),
    strip.background = element_rect(fill = "white", colour = "black", linewidth = 1)
  )
```

<div class="figure">

<img src="v02-Scars-PD-and-AOB_files/figure-gfm/fa-per-flake-type-1.png" alt="Fracture angle values according to platform depth, AOB and flake type"  />
<p class="caption">

<span id="fig:fa-per-flake-type"></span>Figure 8: Fracture angle values
according to platform depth, AOB and flake type
</p>

</div>

 

## **4) Discussion**

Accurately predicting flake mass (and understanding which variables
control this outcome) has long been a major goal in lithic studies.
Mechanical flaking experiments have been essential for establishing
controlled conditions in which individual variables can be manipulated
to assess their effects. These experiments demonstrated that using just
three variables (PD, EPA, and AOB) can yield “extremely large”
\[[32](#ref-hiscockGeneralizationInferenceQuantification2010)\] linear
correlations above 0.8 when predicting flake mass
\[[36](#ref-dibbleEffectHammerMass1995),[40](#ref-pelcinControlledExperimentsProduction1996)\].
However, this high level of accuracy did not translate to freehand
experiments or archaeological assemblages
\[[56](#ref-davisQuantifyingLithicCuration1998)\]. Subsequent research
sought to address this gap by modifying and clarifying the variables
employed to predict flake mass
\[[57](#ref-shottFlakeSizePlatform2000a)\], improving measurement
protocols
\[[59](#ref-mullerNewMethodAccurately2016)–[61](#ref-clarksonEstimatingOriginalFlake2011)\],
and incorporating additional variables or alternative predictive
algorithms
\[[54](#ref-bustos-perezMultipleApproachesPredicting2022),[55](#ref-bustos-perezPredictingFlakeMass2021),[58](#ref-dogandzicEdgeLengthSurface2015),[80](#ref-maloneyExperimentalArchaeologicalTesting2020a),[81](#ref-shottUseMultifactorialReconciliation2017)\].

 

With a few exceptions
\[[40](#ref-pelcinControlledExperimentsProduction1996)\], most
mechanical flaking experiments detached only a single flake from each
core. As a result, the flakes produced were effectively cortical flakes.
This poses a limitation when applying this model to freehand experiments
or archaeological assemblages where most flakes have scars from previous
removals and therefore vary widely in scar configuration and the amount
of mass already removed. The present study directly addresses this issue
by producing both primary and secondary flakes (capturing the presence
or absence of previous scars) within the same controlled mechanical
flaking framework. This design made it possible to establish a baseline
model (equivalent to that of previous studies) using primary flakes
(primary‑to‑primary), extend this model to predict the mass of secondary
flakes (primary‑to‑secondary), and evaluate whether a model trained
specifically on secondary flakes (secondary‑to‑secondary) could achieve
predictive performance comparable to the baseline.

 

The primary-to-primary predictions resulted in an “extremely large”
linear correlation when predicting flake mass ($r^2$ = 0.831). The
metrics of how far predictions fall from the true value (RMSE and MAE)
indicate in general a very accurate model. The density plot and analysis
of residuals indicates that the model might be prone to slightly
overestimate flake mass. Initial evaluation indicates that this model
does not present any bias along the range of predictions, and even more
importantly, no bias is present along the different values of PD.
However, initial analysis of the primary flakes indicated that larger
cores result in larger and heavier flakes
\[[64](#ref-bustos-perezRulesAreRules2026)\]. This translates into a
bias of the primary-to-primary model in which flakes detached from the
smaller cores had their mass estimations slightly overestimated, while
flakes detached from the larger cores had their mass slightly
underestimated. Despite this bias due to initial core size, these
results point out that a model using these variables provided accurate,
unbiased predictions.

 

The linear correlation coefficient obtained for the present data is
similar to those reported in previously mechanical flaking experiments.
Dibble and Pelcin \[[36](#ref-dibbleEffectHammerMass1995)\] used glass
plates and a drop tower with steel balls to detach flakes from glass
plates. They note the non-linear relationship between the variables and
the outcome, and provided an equation to account for this, obtaining an
$r^2$ value of 0.815. Later, a synthesis of the controlled glass
experiments carried out by Dibble and colleagues over the years
\[[46](#ref-liSynthesisDibbleControlled2023)\] presented additional
results. For flakes equivalent to those in this experiment, an $r^2$ of
0.8 was reported for the model not including AOB and 0.9 when this
variable was added. The linear correlation results presented here fall
between these two values.

Lin et al. \[[66](#ref-linControllingLevalloisEffect2025)\] evaluated
the effects of PD and AOB on standardized glass Levallois flakes,
keeping EPA constant at 75° while varying PD and AOB. Importantly, all
flakes were detached from the same core mold, meaning that the dorsal
scar configuration was identical across specimens. As a result, their
experiment did not account for variation in dorsal scars and variations
of mass removed. For predicting the cube‑root mass of these Levallois
flakes, they reported an $r^2$ of 0.86, almost identical to the value
obtained here for the baseline primary‑to‑primary model (0.831).
Notably, the PD estimate reported by
\[[66](#ref-linControllingLevalloisEffect2025)\] (0.34) is higher than
the PD estimates obtained in this study for both the primary‑to‑primary
(0.215) and secondary‑to‑secondary (0.161) models. One possible
explanation is that Levallois flakes typically exhibit a highly
structured dorsal scar pattern designed to predetermine flake morphology
and maximize the surface area removed from the core
\[[82](#ref-boedaLevalloisVolumetricConstruction1995),[83](#ref-lycettLevalloisEconomicsExamination2013)\].
This controlled and optimized scar configuration may underlie the higher
PD estimate observed \[[66](#ref-linControllingLevalloisEffect2025)\].

 

When the model trained on primary flakes, comparable to those used in
previous mechanical flaking experiments
\[[37](#ref-dibbleIntroducingNewExperimental2009a),[46](#ref-liSynthesisDibbleControlled2023)\],
was used to predict the mass of secondary flakes (primary to secondary),
all accuracy metrics declined noticeably and clear biases appeared. The
reduction in accuracy is reflected in the lower $r^2$ (from 0.831 to
0.7) and in the increase of the MAE (from 0.175 to 0.224), RMSE (from
0.214 to 0.278), and MAPE (12.91). In addition to this loss of accuracy,
a marked bias was observed across the range of predictions and with
respect to PD (Figure <a href="#fig:res-plots">6</a>). In general terms,
the larger the PD, the more strongly the model overestimated flake mass.
Results from the secondary-to-secondary model were mixed. Its $r^2$
(0.705), MAE (0.193) and RMSE (0.245) values fall between those of the
baseline and the primary-to-secondary models, indicating lower
predictive precision than the baseline but better performance than the
cross‑group predictions. As with the baseline model, no systematic bias
was detected across the range of predictions or in relation to PD
values.

 

The most plausible explanation is that, at the moment the fracture
initiates, secondary flakes simply have less available mass to remove
from their dorsal surface when the fracture plane travels in the same
way it would on a primary flake. PD, EPA, and AOB act as the initiators
and directors of the fracture plane, determining how far below the
surface it propagates, how subparallel it travels relative to the
exterior of the core, and how quickly it exits. These factors largely
govern the size, mass, and morphology of the resulting flake.
Consequently, the three variables retain strong predictive power for
flake mass even when part of the dorsal mass has already been removed,
as in the primary‑to‑secondary and secondary‑to‑secondary predictions.

 

The analysis of fracture initiation variables related with PD (PW, PSIA
and platform surface) shows little difference between primary and
secondary flakes. For the PW, and PSIA, no difference is present between
the flake types. Secondary flakes presented slightly larger platforms,
but PD remains the main determinant for platform size. The absence of
substantial differences in variables related with platform dimensions
and fracture initiation suggests that fracture initiation was broadly
comparable across flake types. Once the fracture begins and travels
through the core, its trajectory is independent of how much mass remains
on the dorsal face; the missing mass from previous removals does not
alter the fracture path itself. It does not, for instance, cut further
into the core to equalize the amount of mass removed for a given PD, EPA
and AOB. The analysis of flake linear measurements reinforces this
interpretation. Flake length is not significantly affected by flake type
when predicted from PD, AOB, and fracture angle, while secondary flakes
tend to be slightly thicker than primary flakes. Width, however, is the
measurement most strongly affected. In the experimental setup, the
fracture intersects scars from previous removals along the lateral
margins. When these scars lie topographically below the advancing
fracture surface, the fracture exits the core laterally while continuing
its forward propagation. This produces flakes that are narrower than the
primary removals. The PD and AOB coefficient estimates across both
models support this interpretation: given the same PD value, more mass
will be removed from a primary flake than from a secondary flake (since
these flakes will be wider). The residual analysis for the
primary‑to‑secondary predictions further reinforces this scenario.
Larger flakes (with bigger PD’s) tend to capture a greater proportion of
previous scars, and as a result, the discrepancy between the mass they
should have and the mass they actually retain increases. This helps
explain why experiments (either mechanical or hand held) that rely on
these variables, or on derivatives of them, consistently produce strong
linear correlations. It also helps clarify why studies using these same
variables appear to be reaching a limit in the accuracy of their
predictions \[[54](#ref-bustos-perezMultipleApproachesPredicting2022)\].

 

As previously noted, one of the main limitations of mechanical flaking
experiments is that the high accuracy achieved when predicting flake
mass does not translate well to freehand experiments or archaeological
assemblages. Davis and Shea
\[[56](#ref-davisQuantifyingLithicCuration1998)\] applied the Dibble and
Pelcin \[[36](#ref-dibbleEffectHammerMass1995)\] equation, which uses PD
and EPA, to estimate flake mass. Their resulting $r^2$ value (0.712) was
much lower than the previously reported
\[[36](#ref-dibbleEffectHammerMass1995)\] and is more comparable to the
primary-to-secondary and secondary-to-secondary predictions obtained in
the present study. Davis and Shea
\[[56](#ref-davisQuantifyingLithicCuration1998)\] also observed that the
previously noted equation \[[36](#ref-dibbleEffectHammerMass1995)\]
tended to underestimate flake mass, whereas the results presented here
show an overestimation when primary flakes are used to predict the mass
of secondary flakes. Shott et al.
\[[57](#ref-shottFlakeSizePlatform2000a)\] similarly applied several
Pelcin equations
\[[36](#ref-dibbleEffectHammerMass1995),[40](#ref-pelcinControlledExperimentsProduction1996)\]
to two experimentally produced chert assemblages. In both cases, the
formulas performed poorly, and overestimation of flake mass was common.

 

The results presented here offer a possible explanation for the reduced
correlation between mechanical flaking experiments and freehand or
archaeological assemblages. As shown, the predictive model generated
from primary flakes does not generalize well to secondary flakes, as
indicated by the decrease in precision metrics and the presence of
PD‑related bias. The Dibble and Pelcin formula
\[[36](#ref-dibbleEffectHammerMass1995)\] was derived from flakes
produced on glass plates, which are effectively equivalent to primary
flakes. In contrast, Davis and Shea
\[[56](#ref-davisQuantifyingLithicCuration1998)\] used obsidian nodules,
and Shott et al. \[[57](#ref-shottFlakeSizePlatform2000a)\] used chert
nodules, both of which produce mixtures of primary and secondary flakes.
It is therefore unlikely that the model and equation proposed
\[[36](#ref-dibbleEffectHammerMass1995)\] would generalize effectively
to the hand made flakes
\[[56](#ref-davisQuantifyingLithicCuration1998),[57](#ref-shottFlakeSizePlatform2000a)\].

 

Pelcin \[[40](#ref-pelcinControlledExperimentsProduction1996)\] offers a
somewhat different perspective. Using multiple strike‑face glass cores
he produced both primary and secondary flakes while keeping EPA and AOB
constant at 75° and 40° respectively. Although his analysis was largely
exploratory, he reported no major differences between the two flake
types, noting only that secondary flakes showed greater variability in
their linear attributes. For primary flakes, he obtained a linear
regression $r^2$ of 0.851 for platform thickness, while the
corresponding value for secondary flakes was 0.944. When both flake
types were combined, the regression on platform thickness yielded an
$r^2$ of 0.908. The results presented here are partially consistent with
these observations. On one hand, as noted above, PD retains strong
predictive power for both primary and secondary flakes (the $r^2$
obtained for primary flakes is similar in both experiments). The
discrepancy appears in the secondary flakes, where neither the
primary‑to‑secondary nor the secondary‑to‑secondary models produced
linear correlations comparable to those previously reported
\[[40](#ref-pelcinControlledExperimentsProduction1996)\]. Two underlying
reasons might be the cause of this. First, Pelcin
\[[40](#ref-pelcinControlledExperimentsProduction1996)\] evaluated the
ability of PD to predict the mass of primary and secondary flakes
directly, but did not test whether a model trained on primary flakes
could be reliably extended to secondary flakes. Second, his flakes were
detached at an AOB of 40°, whereas the flakes in the present study were
detached at more perpendicular AOBs, which cause the fracture to
penetrate deeper into the core
\[[37](#ref-dibbleIntroducingNewExperimental2009a),[64](#ref-bustos-perezRulesAreRules2026)\].
Because of this deeper penetration, differences in mass between primary
and secondary flakes for a given PD and AOB are more likely to appear in
the present experiment.

 

However, as shown by the primary‑to‑secondary and secondary‑to‑secondary
predictions, PD, EPA, and AOB still retain substantial predictive power
when estimating flake mass. This likely explains why experiments that
rely on these variables, or on derivatives of them, consistently produce
strong linear correlations. At the same time, it may also explain why
studies using these variables appear to have reached a ceiling in
predictive accuracy
\[[54](#ref-bustos-perezMultipleApproachesPredicting2022)\]. The
difficulty in applying the mechanical flaking formulas
\[[36](#ref-dibbleEffectHammerMass1995),[40](#ref-pelcinControlledExperimentsProduction1996)\]
to freehand flakes led Shott et al.,
\[[57](#ref-shottFlakeSizePlatform2000a)\] to use regressions based on
the logarithms of mass and platform area.

 

In this context, it is important to note that PD and platform width are
highly correlated in mechanical flaking experiments
\[[46](#ref-liSynthesisDibbleControlled2023),[65](#ref-leaderEffectsPlatformBeveling2017),[67](#ref-linMediatingEffectPlatform2022a)\].
This strong correlation allows platform width to be removed and PD to
serve as a single proxy for platform size, reducing the number of
predictors and avoiding collinearity. In freehand experiments, however,
this correlation is much weaker due to the greater morphological
variability of platforms, making it advisable to include platform size
explicitly. Consistent with this, several studies using digital
photographs \[[60](#ref-braunLandscapescaleVariationHominin2008)\], 3D
scans \[[61](#ref-clarksonEstimatingOriginalFlake2011)\], or adaptations
of the formula for calculating platform area
\[[55](#ref-bustos-perezPredictingFlakeMass2021),[59](#ref-mullerNewMethodAccurately2016)\]
have reported improved accuracy when predicting the mass of hand‑made
flakes.

 

Some caution regarding the use of variables must be considered when
interpreting the results presented here. We used curved cores that make
it impossible to effectively measure EPA. Instead, we use fracture angle
here but both fracture angle and EPA improve the predictions of flake
mass (EPA is reported to do this in a better way, although fracture
angle can be measured irrespective of dorsal surface topography). A
consequence of this is that the variables used for predicting flake mass
are not exactly the same as in previous experiments
\[[36](#ref-dibbleEffectHammerMass1995),[46](#ref-liSynthesisDibbleControlled2023),[65](#ref-leaderEffectsPlatformBeveling2017)\].
Further research might consider exploring if EPA would account for the
diminishing accuracy and bias of the primary-to-secondary and
secondary-to-secondary.

 

Not all residuals from the secondary flakes indicate a systematic
overestimation of flake mass. In the experimental sample, the scar
configurations from previous removals vary widely. Some secondary flakes
preserve only a single scar, whereas others incorporate two or more.
These scars also differ considerably in size, depth, and length. In
addition, the central convexity of the core and the ridges created by
earlier removals may help guide the fracture forward during detachment.
The combination of these factors can, in some cases, compensate for the
mass missing due to previous removals. As a result, the bias observed in
the residuals appears as a general trend across the dataset rather than
as a consistent, systematic pattern. The specifics of the relationship
between scar morphology and aspects of flake size require further
analysis.

 

Regarding flake formation models, three possible scenarios were proposed
as a consequence of the presence of scars. First, it was proposed that
given a PD, the flake would expand to compensate for the mass missing
due to the scars of previous removals. If this were the case, given the
same PD, a primary and a secondary flake would have approximately the
same mass. However, as already noted, extending predictions from primary
flakes to secondary flakes worsens accuracy metrics and generates
overpredictions for high PD values. This seems to indicate that although
the presence of previous scars can favor the fracture to travel further,
and thus resulting in a longer flake, this does not compensate for the
mass removed in previous extractions.

 

The results obtained also have implications for studies aimed at
reconstructing resharpening intensity among Paleolithic retouched
artifacts. Previous research
\[[32](#ref-hiscockGeneralizationInferenceQuantification2010),[56](#ref-davisQuantifyingLithicCuration1998),[61](#ref-clarksonEstimatingOriginalFlake2011)\]
has often sought to develop a universal reduction index based on a
limited number of variables that can be applied across diverse
assemblages types and periods. However, the present results suggest that
achieving high predictive accuracy at the level of individual artifacts
may be difficult when relying on this restricted set of variables,
particularly given the multiple factors that influence flake formation
and mass detachment. Nevertheless, the strong predictive performance
observed across the experimental assemblage indicates that these
variables retain substantial explanatory power at broader analytical
scales. Consequently, although caution is warranted when interpreting
predictions for individual artifacts, comparisons among assemblages or
groups of artifacts may remain a robust approach for investigating
patterns of resharpening intensity and reduction.

 

## **5) Conclusions**

Mechanical flaking experiments pioneered the proposition that a few
variables are important for predicting flake mass (PD, EPA and AOB).
However, the framework of these mechanical flaking experiments was
limited to flakes without scars from previous removals (primary flakes).
When models trained on primary flakes are applied to flakes with scars
(secondary), all precision metrics decrease and biases indicating
overestimations of flake mass appear. This bias is specially marked in
the case of flakes with bigger platforms (which capture larger portions
of the flake, and thus larger portions of previous scars). Further,
models trained on secondary flakes also do not reach the accuracy of the
primary-to-primary predictions. Despite this, PD and AOB retain their
predictive value for estimating flake mass. Fracture does not compensate
for the presence of scars by increasing the size of the flake until it
reaches a certain mass. Rather, fracture seems to be geometrically
predicted by the core configuration (PD and EPA/FA) and force delivery
(AOB), and mass is simply a biproduct of the fracture path plus the core
surface morphology.

 

## **Acknowledgments**

This research was supported by funding from the Max Planck Society and
conducted in the Department of Human Origins under the direction of
Tracy Kivell. We are very grateful to the department’s technical staff
and colleagues for their assistance during the experimental work.

 

## **Contributions**

Author contributions are described according to CRediT taxonomy. GBP
contributed to conceptualization, data curation, formal analysis,
investigation, methodology, resources, software, validation,
visualization, and to writing both the original draft and subsequent
revisions. SPM participated in these roles and additionally contributed
to funding acquisition, project administration, and supervision,
providing overarching guidance throughout the study.

 

## **6) References**

</div>

<div id="refs" class="references csl-bib-body">

<div id="ref-braunEarliestKnownOldowan2019" class="csl-entry">

<span class="csl-left-margin">1.
</span><span class="csl-right-inline">Braun DR, Aldeias V, Archer W,
Arrowsmith JR, Baraki N, Campisano CJ, et al. Earliest known Oldowan
artifacts at $>$2.58 Ma from Ledi-Geraru, Ethiopia, highlight early
technological diversity. Proceedings of the National Academy of
Sciences. 2019;116: 11712–11717.
doi:[10.1073/pnas.1820177116](https://doi.org/10.1073/pnas.1820177116)</span>

</div>

<div id="ref-harmand33millionyearoldStoneTools2015" class="csl-entry">

<span class="csl-left-margin">2.
</span><span class="csl-right-inline">Harmand S, Lewis JE, Feibel CS,
Lepre CJ, Prat S, Lenoble A, et al. 3.3-million-year-old stone tools
from Lomekwi 3, West Turkana, Kenya. Nature. 2015;521: 310–315.
doi:[10.1038/nature14464](https://doi.org/10.1038/nature14464)</span>

</div>

<div id="ref-plummerExpandedGeographicDistribution2023"
class="csl-entry">

<span class="csl-left-margin">3.
</span><span class="csl-right-inline">Plummer TW, Oliver JS, Finestone
EM, Ditchfield PW, Bishop LC, Blumenthal SA, et al. Expanded geographic
distribution and dietary strategies of the earliest Oldowan hominins and
Paranthropus. Science. 2023;379: 561–566.
doi:[10.1126/science.abo7452](https://doi.org/10.1126/science.abo7452)</span>

</div>

<div id="ref-semawWorldsOldestStone2000" class="csl-entry">

<span class="csl-left-margin">4.
</span><span class="csl-right-inline">Semaw S. The World’s Oldest Stone
Artefacts from Gona, Ethiopia: Their Implications for Understanding
Stone Technology and Patterns of Human Evolution Between
2$\cdot$6–1$\cdot$5 Million Years Ago. Journal of Archaeological
Science. 2000;27: 1197–1214.
doi:[10.1006/jasc.1999.0592](https://doi.org/10.1006/jasc.1999.0592)</span>

</div>

<div id="ref-casamiquelaTemasPatagonicosInteres1978a" class="csl-entry">

<span class="csl-left-margin">5.
</span><span class="csl-right-inline">Casamiquela RM. Temas patagónicos
de interes arqueológico. La talla del vidrio. Relaciones de la Sociedad
Argentina de Antropología. 1978;12: 213–223. </span>

</div>

<div id="ref-gallagherContemporaryStoneTools1977a" class="csl-entry">

<span class="csl-left-margin">6.
</span><span class="csl-right-inline">Gallagher JP. Contemporary Stone
Tools in Ethiopia: Implications for Archaeology. Journal of Field
Archaeology. 1977;4: 407–414.
doi:[10.1179/009346977791490131](https://doi.org/10.1179/009346977791490131)</span>

</div>

<div id="ref-gouldLivingArchaeologyNgatatjaraof1968" class="csl-entry">

<span class="csl-left-margin">7.
</span><span class="csl-right-inline">Gould RA. Living archaeology: The
Ngatatjaraof Western Australia. Southwestern Journal of Anthropology.
1968;24: 101–122. </span>

</div>

<div id="ref-haydenPaleolithicReflectionsLithic1979" class="csl-entry">

<span class="csl-left-margin">8.
</span><span class="csl-right-inline">Hayden B. Paleolithic reflections:
Lithic technology and ethnographic excavations among Australian
aborigines. Camberra: Humanities Press; 1979. </span>

</div>

<div id="ref-haydenStoneToolFunctions1977" class="csl-entry">

<span class="csl-left-margin">9.
</span><span class="csl-right-inline">Hayden B. Stone Tool Functions in
the Western Desert. In: Wrigth RVS, editor. Stone Tools as Cultural
Markers: Change, Evolution, and Complexity. Camberra: Australian
Institute of Aboriginal Studies; 1977. pp. 178–188. </span>

</div>

<div id="ref-sillitoeLivingLithicsEthnoarchaeology2003a"
class="csl-entry">

<span class="csl-left-margin">10.
</span><span class="csl-right-inline">Sillitoe P, Hardy K. Living
Lithics: Ethnoarchaeology in Highland Papua New Guinea. Antiquity.
2003;77: 555–566.
doi:[10.1017/S0003598X00092619](https://doi.org/10.1017/S0003598X00092619)</span>

</div>

<div id="ref-weedmanEthnoarchaeologicalStudyHafting2006a"
class="csl-entry">

<span class="csl-left-margin">11.
</span><span class="csl-right-inline">Weedman KJ. An Ethnoarchaeological
Study of Hafting and Stone Tool Diversity among the Gamo of Ethiopia.
Journal of Archaeological Method and Theory. 2006;13: 188–237.
doi:[10.1007/s10816-006-9010-4](https://doi.org/10.1007/s10816-006-9010-4)</span>

</div>

<div id="ref-kivellAustralopithecusSedibaHand2011" class="csl-entry">

<span class="csl-left-margin">12.
</span><span class="csl-right-inline">Kivell TL, Kibii JM, Churchill SE,
Schmid P, Berger LR. Australopithecus sediba Hand Demonstrates Mosaic
Evolution of Locomotor and Manipulative Abilities. Science. 2011;333:
1411–1417.
doi:[10.1126/science.1202625](https://doi.org/10.1126/science.1202625)</span>

</div>

<div id="ref-marzkeToolMakingHand2013" class="csl-entry">

<span class="csl-left-margin">13.
</span><span class="csl-right-inline">Marzke MW. Tool making, hand
morphology and fossil hominins. Philosophical Transactions of the Royal
Society B: Biological Sciences. 2013;368: 20120414.
doi:[10.1098/rstb.2012.0414](https://doi.org/10.1098/rstb.2012.0414)</span>

</div>

<div id="ref-stoutNeuralCorrelatesEarly2008" class="csl-entry">

<span class="csl-left-margin">14.
</span><span class="csl-right-inline">Stout D, Toth N, Schick K,
Chaminade T. Neural correlates of Early Stone Age toolmaking:
Technology, language and cognition in human evolution. Philosophical
Transactions of the Royal Society B: Biological Sciences. 2008;363:
1939–1949.
doi:[10.1098/rstb.2008.0001](https://doi.org/10.1098/rstb.2008.0001)</span>

</div>

<div id="ref-tothOldowanReassessedClose1985" class="csl-entry">

<span class="csl-left-margin">15.
</span><span class="csl-right-inline">Toth N. The oldowan reassessed: A
close look at early stone artifacts. Journal of Archaeological Science.
1985;12: 101–120.
doi:[10.1016/0305-4403(85)90056-1](https://doi.org/10.1016/0305-4403(85)90056-1)</span>

</div>

<div id="ref-geribasWhatNoviceKnappers2010" class="csl-entry">

<span class="csl-left-margin">16.
</span><span class="csl-right-inline">Geribàs N, Mosquera M, Vergès JM.
What novice knappers have to learn to become expert stone toolmakers.
Journal of Archaeological Science. 2010;37: 2857–2870.
doi:[10.1016/j.jas.2010.06.026](https://doi.org/10.1016/j.jas.2010.06.026)</span>

</div>

<div id="ref-lombaoTeachingMakeStone2017" class="csl-entry">

<span class="csl-left-margin">17.
</span><span class="csl-right-inline">Lombao D, Guardiola M, Mosquera M.
Teaching to make stone tools: New experimental evidence supporting a
technological hypothesis for the origins of language. Scientific
Reports. 2017;7: 14394.
doi:[10.1038/s41598-017-14322-y](https://doi.org/10.1038/s41598-017-14322-y)</span>

</div>

<div id="ref-nonakaHowStoneKnappers2010" class="csl-entry">

<span class="csl-left-margin">18.
</span><span class="csl-right-inline">Nonaka T, Bril B, Rein R. How do
stone knappers predict and control the outcome of flaking? Implications
for understanding early stone tool technology. Journal of Human
Evolution. 2010;59: 155–167.
doi:[10.1016/j.jhevol.2010.04.006](https://doi.org/10.1016/j.jhevol.2010.04.006)</span>

</div>

<div id="ref-pargeterTestingEffectLearning2023" class="csl-entry">

<span class="csl-left-margin">19.
</span><span class="csl-right-inline">Pargeter J, Liu C, Kilgore MB,
Majoe A, Stout D. Testing the Effect of Learning Conditions and
Individual Motor/Cognitive Differences on Knapping Skill Acquisition.
Journal of Archaeological Method and Theory. 2023;30: 127–171.
doi:[10.1007/s10816-022-09592-4](https://doi.org/10.1007/s10816-022-09592-4)</span>

</div>

<div id="ref-debenathHandbookPaleolithicTypology1994" class="csl-entry">

<span class="csl-left-margin">20.
</span><span class="csl-right-inline">Debénath A, Dibble HL. Handbook of
Paleolithic Typology: Lower and middle paleolithic of Europe (Vol. 1).
University of Pennsylvania Press; 1994. </span>

</div>

<div id="ref-kuhnEvolutionPaleolithicTechnologies2020"
class="csl-entry">

<span class="csl-left-margin">21.
</span><span class="csl-right-inline">Kuhn SL. The Evolution of
Paleolithic Technologies. 1st ed. Abingdon, Oxon; New York, NY :
Routledge, 2020. : Routledge; 2020.
doi:[10.4324/9781315642024](https://doi.org/10.4324/9781315642024)</span>

</div>

<div id="ref-rezekTwoMillionYears2018" class="csl-entry">

<span class="csl-left-margin">22.
</span><span class="csl-right-inline">Režek Ž, Dibble HL, McPherron SP,
Braun DR, Lin SC. Two million years of flaking stone and the
evolutionary efficiency of stone tool technology. Nature Ecology &
Evolution. 2018;2: 628–633.
doi:[10.1038/s41559-018-0488-4](https://doi.org/10.1038/s41559-018-0488-4)</span>

</div>

<div id="ref-andrefskyRawMaterialAvailabilityOrganization1994"
class="csl-entry">

<span class="csl-left-margin">23.
</span><span class="csl-right-inline">Andrefsky W. Raw-Material
Availability and the Organization of Technology. American Antiquity.
1994;59: 21–34. </span>

</div>

<div id="ref-bartonFormationLithicAssemblages2014" class="csl-entry">

<span class="csl-left-margin">24.
</span><span class="csl-right-inline">Barton CM, Riel-Salvatore J. The
formation of lithic assemblages. Journal of Archaeological Science.
2014;46: 334–352.
doi:[10.1016/j.jas.2014.03.031](https://doi.org/10.1016/j.jas.2014.03.031)</span>

</div>

<div id="ref-kuhnFormalApproachDesign1994" class="csl-entry">

<span class="csl-left-margin">25.
</span><span class="csl-right-inline">Kuhn SL. A Formal Approach to the
Design and Assembly of Mobile Toolkits. American Antiquity. 1994;59:
426–442. doi:[10.2307/282456](https://doi.org/10.2307/282456)</span>

</div>

<div id="ref-dibbleInterpretationMiddlePaleolithic1987a"
class="csl-entry">

<span class="csl-left-margin">26.
</span><span class="csl-right-inline">Dibble HL. The interpretation of
Middle Paleolithic scraper morphology. American Antiquity. 1987;52:
109–117. </span>

</div>

<div id="ref-dibbleMiddlePaleolithicScraper1995a" class="csl-entry">

<span class="csl-left-margin">27.
</span><span class="csl-right-inline">Dibble HL. Middle Paleolithic
Scraper Reduction: Background, Clarification, and Review of the Evidence
to Date. Journal of Archaeological Method and Theory. 1995;2: 300–368.
</span>

</div>

<div id="ref-kuhnMousterianLithicTechnology1995" class="csl-entry">

<span class="csl-left-margin">28.
</span><span class="csl-right-inline">Kuhn SL. Mousterian Lithic
Technology. Princeton: Princeton University Press; 1995. </span>

</div>

<div id="ref-shottCostsBenefitsTechnological2018" class="csl-entry">

<span class="csl-left-margin">29.
</span><span class="csl-right-inline">Shott MJ. The Costs and Benefits
of Technological Organization: Hunter-Gatherer Lithic Industries and
Beyond. In: Robinson E, Sellet F, editors. Lithic Technological
Organization and Paleoenvironmental Change: Global and Diachronic
Perspectives. Cham: Springer International Publishing; 2018. pp.
321–333.
doi:[10.1007/978-3-319-64407-3_15](https://doi.org/10.1007/978-3-319-64407-3_15)</span>

</div>

<div id="ref-shottExegesisCurationConcept1996" class="csl-entry">

<span class="csl-left-margin">30.
</span><span class="csl-right-inline">Shott MJ. An Exegesis of the
Curation Concept. Journal of Anthropological Research. 1996;52: 259–280.
</span>

</div>

<div id="ref-shottUseLifeCuration2005" class="csl-entry">

<span class="csl-left-margin">31.
</span><span class="csl-right-inline">Shott MJ, Sillitoe P. Use life and
curation in New Guinea experimental used flakes. Journal of
Archaeological Science. 2005;32: 653–663.
doi:[10.1016/j.jas.2004.11.012](https://doi.org/10.1016/j.jas.2004.11.012)</span>

</div>

<div id="ref-hiscockGeneralizationInferenceQuantification2010"
class="csl-entry">

<span class="csl-left-margin">32.
</span><span class="csl-right-inline">Hiscock P, Tabrett A.
Generalization, inference and the quantification of lithic reduction.
World Archaeology. 2010;42: 545–561.
doi:[10.1080/00438243.2010.517669](https://doi.org/10.1080/00438243.2010.517669)</span>

</div>

<div id="ref-cotterellEssentialMechanicsConchoidal1985"
class="csl-entry">

<span class="csl-left-margin">33.
</span><span class="csl-right-inline">Cotterell B, Kamminga J, Dickson
FP. The essential mechanics of conchoidal flaking. International Journal
of Fracture. 1985;29: 205–221.
doi:[10.1007/BF00125471](https://doi.org/10.1007/BF00125471)</span>

</div>

<div id="ref-cotterellFormationFlakes1987" class="csl-entry">

<span class="csl-left-margin">34.
</span><span class="csl-right-inline">Cotterell B, Kamminga J. The
Formation of Flakes. American Antiquity. 1987;52: 675–708.
doi:[10.2307/281378](https://doi.org/10.2307/281378)</span>

</div>

<div id="ref-cotterellMechanicsPreindustrialTechnology1990"
class="csl-entry">

<span class="csl-left-margin">35.
</span><span class="csl-right-inline">Cotterell B, Kamminga J. Mechanics
of pre-industrial technology: An introduction to the mechanics of
ancient and traditional material culture. Cambridge University Press;
1990. </span>

</div>

<div id="ref-dibbleEffectHammerMass1995" class="csl-entry">

<span class="csl-left-margin">36.
</span><span class="csl-right-inline">Dibble HL, Pelcin A. The Effect of
Hammer Mass and Velocity on Flake Mass. Journal of Archaeological
Science. 1995;22: 429–439.
doi:[10.1006/jasc.1995.0042](https://doi.org/10.1006/jasc.1995.0042)</span>

</div>

<div id="ref-dibbleIntroducingNewExperimental2009a" class="csl-entry">

<span class="csl-left-margin">37.
</span><span class="csl-right-inline">Dibble HL, Rezek Z. Introducing a
new experimental design for controlled studies of flake formation:
Results for exterior platform angle, platform depth, angle of blow,
velocity, and force. Journal of Archaeological Science. 2009;36:
1945–1954.
doi:[10.1016/j.jas.2009.05.004](https://doi.org/10.1016/j.jas.2009.05.004)</span>

</div>

<div id="ref-faulknerMechanicalPrinciplesFlintworking1972"
class="csl-entry">

<span class="csl-left-margin">38.
</span><span class="csl-right-inline">Faulkner A. Mechanical principles
of flintworking. PhD thesis, Washington State University. 1972. </span>

</div>

<div id="ref-faulknerMechanicsEraillureFormation1973" class="csl-entry">

<span class="csl-left-margin">39.
</span><span class="csl-right-inline">Faulkner A. Mechanics of Eraillure
Formation. Newsletter of Lithic Technology. 1973;2: 4–12. Available:
<https://www.jstor.org/stable/43822284></span>

</div>

<div id="ref-pelcinControlledExperimentsProduction1996"
class="csl-entry">

<span class="csl-left-margin">40.
</span><span class="csl-right-inline">Pelcin AW. Controlled experiments
in the production of flake attributes. PhD thesis, University of
Pennsylvania. 1996. </span>

</div>

<div id="ref-pelcinEffectCoreSurface1997a" class="csl-entry">

<span class="csl-left-margin">41.
</span><span class="csl-right-inline">Pelcin AW. The Effect of Core
Surface Morphology on Flake Attributes: Evidence from a Controlled
Experiment. Journal of Archaeological Science. 1997;24: 749–756.
doi:[10.1006/jasc.1996.0156](https://doi.org/10.1006/jasc.1996.0156)</span>

</div>

<div id="ref-pelcinEffectIndentorType1997" class="csl-entry">

<span class="csl-left-margin">42.
</span><span class="csl-right-inline">Pelcin A. The Effect of Indentor
Type on Flake Attributes: Evidence from a Controlled Experiment. Journal
of Archaeological Science. 1997;24: 613–621.
doi:[10.1006/jasc.1996.0145](https://doi.org/10.1006/jasc.1996.0145)</span>

</div>

<div id="ref-pelcinFormationFlakesRole1997" class="csl-entry">

<span class="csl-left-margin">43.
</span><span class="csl-right-inline">Pelcin AW. The Formation of
Flakes: The Role of Platform Thickness and Exterior Platform Angle in
the Production of Flake Initiations and Terminations. Journal of
Archaeological Science. 1997;24: 1107–1113.
doi:[10.1006/jasc.1996.0190](https://doi.org/10.1006/jasc.1996.0190)</span>

</div>

<div id="ref-vanpeerMechanicalFrameworkConchoidal2021"
class="csl-entry">

<span class="csl-left-margin">44.
</span><span class="csl-right-inline">Van Peer P. A Mechanical Framework
of Conchoidal Flaking and Its Place in Lithic Systematics. Journal of
Paleolithic Archaeology. 2021;4: 8.
doi:[10.1007/s41982-021-00086-5](https://doi.org/10.1007/s41982-021-00086-5)</span>

</div>

<div id="ref-dibblePlatformVariabilityFlake1997" class="csl-entry">

<span class="csl-left-margin">45.
</span><span class="csl-right-inline">Dibble HL. Platform variability
and flake morphology: A comparison of experimental and archaeological
data and implications for interpreting prehistoric lithic technological
strategies. Lithic Technology. 1997;22: 150–170. </span>

</div>

<div id="ref-liSynthesisDibbleControlled2023" class="csl-entry">

<span class="csl-left-margin">46.
</span><span class="csl-right-inline">Li L, Lin SC, McPherron SP,
Abdolahzadeh A, Chan A, Dogandžić T, et al. A Synthesis of the Dibble et
al. Controlled Experiments into the Mechanics of Lithic Production.
Journal of Archaeological Method and Theory. 2023;30: 1284–1325.
doi:[10.1007/s10816-022-09586-2](https://doi.org/10.1007/s10816-022-09586-2)</span>

</div>

<div id="ref-andrefskyLithicsMacroscopicApproaches2005"
class="csl-entry">

<span class="csl-left-margin">47.
</span><span class="csl-right-inline">Andrefsky W. Lithics: Macroscopic
approaches to analysis. Second edition. Cambridge, UK New York:
Cambridge University Press; 2005. </span>

</div>

<div id="ref-cochraneMeasurementAnalysisPlatform2003" class="csl-entry">

<span class="csl-left-margin">48.
</span><span class="csl-right-inline">Cochrane GWG. On the Measurement
and Analysis of Platform Angles. Lithic Technology. 2003;28: 13–25.
Available: <https://www.jstor.org/stable/23273437></span>

</div>

<div id="ref-stoutUnderstandingOldowanKnapping2009" class="csl-entry">

<span class="csl-left-margin">49.
</span><span class="csl-right-inline">Stout D, Schick K, Toth N.
Understanding Oldowan knapping skill: An experimental study of skill
acquisition in modern humans. In: Schick K, Toth N, editors. The cutting
edge: New Approaches to the Archaeology of Human Origins. Stone Age
Institute Press; 2009. pp. 247–266. </span>

</div>

<div id="ref-liQuantifyingKnappingActions2022" class="csl-entry">

<span class="csl-left-margin">50.
</span><span class="csl-right-inline">Li L, Reeves JS, Lin SC, Tennie C,
McPherron SP. Quantifying knapping actions: A method for measuring the
angle of blow on flakes. Archaeological and Anthropological Sciences.
2022;14: 156.
doi:[10.1007/s12520-022-01626-y](https://doi.org/10.1007/s12520-022-01626-y)</span>

</div>

<div id="ref-yezzi-woodley2021" class="csl-entry">

<span class="csl-left-margin">51.
</span><span class="csl-right-inline">Yezzi-Woodley K, Calder J, Olver
PJ, Cody P, Huffstutler T, Terwilliger A, et al. The virtual goniometer:
Demonstrating a new method for measuring angles on archaeological
materials using fragmentary bone. Archaeological and Anthropological
Sciences. 2021;13: 106.
doi:[10.1007/s12520-021-01335-y](https://doi.org/10.1007/s12520-021-01335-y)</span>

</div>

<div id="ref-cignoniMeshLabOpenSourceMesh2008" class="csl-entry">

<span class="csl-left-margin">52.
</span><span class="csl-right-inline">Cignoni P, Callieri M, Corsini M,
Dellepiane M, Ganovelli F, Ranzuglia G. MeshLab: An Open-Source Mesh
Processing Tool. In: Scarano V, De Chiara R, Erra U, editors.
Eurographics Italian Chapter Conference. Eurographics; 2008. pp.
129–136. </span>

</div>

<div id="ref-liDidEarlyPleistocene2023" class="csl-entry">

<span class="csl-left-margin">53.
</span><span class="csl-right-inline">Li L, Reeves JS, Lin SC, Braun DR,
McPherron SP. Did Early Pleistocene hominins control hammer strike
angles when making stone tools? Journal of Human Evolution. 2023;183:
103427.
doi:[10.1016/j.jhevol.2023.103427](https://doi.org/10.1016/j.jhevol.2023.103427)</span>

</div>

<div id="ref-bustos-perezMultipleApproachesPredicting2022"
class="csl-entry">

<span class="csl-left-margin">54.
</span><span class="csl-right-inline">Bustos-Pérez G, Baena Preysler J.
Multiple approaches to predicting flake mass. Journal of Archaeological
Science: Reports. 2022;46: 103698.
doi:[10.1016/j.jasrep.2022.103698](https://doi.org/10.1016/j.jasrep.2022.103698)</span>

</div>

<div id="ref-bustos-perezPredictingFlakeMass2021" class="csl-entry">

<span class="csl-left-margin">55.
</span><span class="csl-right-inline">Bustos-Pérez G, Baena J.
Predicting Flake Mass: A View from Machine Learning. Lithic Technology.
2021;46: 130–142.
doi:[10.1080/01977261.2021.1881267](https://doi.org/10.1080/01977261.2021.1881267)</span>

</div>

<div id="ref-davisQuantifyingLithicCuration1998" class="csl-entry">

<span class="csl-left-margin">56.
</span><span class="csl-right-inline">Davis ZJ, Shea JJ. Quantifying
Lithic Curation: An Experimental Test of Dibble and Pelcin’s Original
Flake-Tool Mass Predictor. Journal of Archaeological Science. 1998;25:
603–610.
doi:[10.1006/jasc.1997.0255](https://doi.org/10.1006/jasc.1997.0255)</span>

</div>

<div id="ref-shottFlakeSizePlatform2000a" class="csl-entry">

<span class="csl-left-margin">57.
</span><span class="csl-right-inline">Shott MJ, Bradbury AP, Carr PJ,
Odell GH. Flake Size from Platform Attributes: Predictive and Empirical
Approaches. Journal of Archaeological Science. 2000;27: 877–894.
doi:[10.1006/jasc.1999.0499](https://doi.org/10.1006/jasc.1999.0499)</span>

</div>

<div id="ref-dogandzicEdgeLengthSurface2015" class="csl-entry">

<span class="csl-left-margin">58.
</span><span class="csl-right-inline">Dogandžić T, Braun DR, McPherron
SP. Edge Length and Surface Area of a Blank: Experimental Assessment of
Measures, Size Predictions and Utility. PLoS ONE. 2015;10: e0133984.
doi:[10.1371/journal.pone.0133984](https://doi.org/10.1371/journal.pone.0133984)</span>

</div>

<div id="ref-mullerNewMethodAccurately2016" class="csl-entry">

<span class="csl-left-margin">59.
</span><span class="csl-right-inline">Muller A, Clarkson C. A new method
for accurately and precisely measuring flake platform area. Journal of
Archaeological Science: Reports. 2016;8: 178–186.
doi:[10.1016/j.jasrep.2016.06.015](https://doi.org/10.1016/j.jasrep.2016.06.015)</span>

</div>

<div id="ref-braunLandscapescaleVariationHominin2008" class="csl-entry">

<span class="csl-left-margin">60.
</span><span class="csl-right-inline">Braun DR, Rogers MJ, Harris JWK,
Walker SJ. Landscape-scale variation in hominin tool use: Evidence from
the Developed Oldowan. Journal of Human Evolution. 2008;55: 1053–1063.
doi:[10.1016/j.jhevol.2008.05.020](https://doi.org/10.1016/j.jhevol.2008.05.020)</span>

</div>

<div id="ref-clarksonEstimatingOriginalFlake2011" class="csl-entry">

<span class="csl-left-margin">61.
</span><span class="csl-right-inline">Clarkson C, Hiscock P. Estimating
original flake mass from 3D scans of platform area. Journal of
Archaeological Science. 2011;38: 1062–1068.
doi:[10.1016/j.jas.2010.12.001](https://doi.org/10.1016/j.jas.2010.12.001)</span>

</div>

<div id="ref-rezekRelativeEffectsCore2011" class="csl-entry">

<span class="csl-left-margin">62.
</span><span class="csl-right-inline">Rezek Z, Lin S, Ioviţă R, Dibble
HL. The relative effects of core surface morphology on flake shape and
other attributes. Journal of Archaeological Science. 2011;38: 1346–1359.
doi:[10.1016/j.jas.2011.01.014](https://doi.org/10.1016/j.jas.2011.01.014)</span>

</div>

<div id="ref-sullivanDebitageAnalysisArchaeological1985"
class="csl-entry">

<span class="csl-left-margin">63.
</span><span class="csl-right-inline">Sullivan AP, Rozen KC. Debitage
Analysis and Archaeological Interpretation. Am antiq. 1985;50: 755–779.
doi:[10.2307/280165](https://doi.org/10.2307/280165)</span>

</div>

<div id="ref-bustos-perezRulesAreRules2026" class="csl-entry">

<span class="csl-left-margin">64.
</span><span class="csl-right-inline">Bustos-Pérez G, McPherron SP. The
Rules are the Rules. Consistency of Drivers of Flake Size and Shape in
Mechanical Flaking Experiments. Lithic Technology. 2026; 1–17.
doi:[10.1080/01977261.2026.2732584](https://doi.org/10.1080/01977261.2026.2732584)</span>

</div>

<div id="ref-leaderEffectsPlatformBeveling2017" class="csl-entry">

<span class="csl-left-margin">65.
</span><span class="csl-right-inline">Leader G, Abdolahzadeh A, Lin SC,
Dibble HL. The effects of platform beveling on flake variation. Journal
of Archaeological Science: Reports. 2017;16: 213–223.
doi:[10.1016/j.jasrep.2017.09.026](https://doi.org/10.1016/j.jasrep.2017.09.026)</span>

</div>

<div id="ref-linControllingLevalloisEffect2025" class="csl-entry">

<span class="csl-left-margin">66.
</span><span class="csl-right-inline">Lin SC, McNaughton M, Innes A,
Barroso-Medina C, Clarkson C. Controlling Levallois: The effect of
hammer angle of blow on Levallois flake morphology and fracture
trajectory. Archaeological and Anthropological Sciences. 2025;17: 107.
doi:[10.1007/s12520-025-02222-6](https://doi.org/10.1007/s12520-025-02222-6)</span>

</div>

<div id="ref-linMediatingEffectPlatform2022a" class="csl-entry">

<span class="csl-left-margin">67.
</span><span class="csl-right-inline">Lin SC, Rezek Z, Abdolahzadeh A,
Braun DR, Dogandžić T, Leader GM, et al. The mediating effect of
platform width on the size and shape of stone flakes. PLOS ONE. 2022;17:
e0262920.
doi:[10.1371/journal.pone.0262920](https://doi.org/10.1371/journal.pone.0262920)</span>

</div>

<div id="ref-linUtilityEconomizationUnretouched2013" class="csl-entry">

<span class="csl-left-margin">68.
</span><span class="csl-right-inline">Lin SC, Rezek Z, Braun D, Dibble
HL. On the Utility and Economization of Unretouched Flakes: The effects
of exterior platform angle and platform depth. American Antiquity.
2013;78: 724–745.
doi:[10.2307/43184970](https://doi.org/10.2307/43184970)</span>

</div>

<div id="ref-schlagerMorphoRvcgShape2017" class="csl-entry">

<span class="csl-left-margin">69.
</span><span class="csl-right-inline">Schlager S. Morpho and Rvcg –
Shape Analysis in R. In: Zheng G, Li S, Székely G, editors. Statistical
Shape and Deformation Analysis. Elsevier; 2017. pp. 217–256.
doi:[10.1016/B978-0-12-810493-4.00011-0](https://doi.org/10.1016/B978-0-12-810493-4.00011-0)</span>

</div>

<div id="ref-mcpherronIntroducingPlatformSurface2020" class="csl-entry">

<span class="csl-left-margin">70.
</span><span class="csl-right-inline">McPherron SP, Abdolahzadeh A,
Archer W, Chan A, Djakovic I, Dogandžić T, et al. Introducing platform
surface interior angle (PSIA) and its role in flake formation, size and
shape. Wang J, editor. PLOS ONE. 2020;15: e0241714.
doi:[10.1371/journal.pone.0241714](https://doi.org/10.1371/journal.pone.0241714)</span>

</div>

<div id="ref-jamesIntroductionStatisticalLearning2013"
class="csl-entry">

<span class="csl-left-margin">71.
</span><span class="csl-right-inline">James G, Witten D, Hastie T,
Tibshirani R. An Introduction to Statistical Learning with Applications
in R. Second Edition. New York: springer; 2013. </span>

</div>

<div id="ref-anscombeGraphsStatisticalAnalysis1973" class="csl-entry">

<span class="csl-left-margin">72.
</span><span class="csl-right-inline">Anscombe FJ. Graphs in Statistical
Analysis. The American Statistician. 1973;27: 17.
doi:[10.2307/2682899](https://doi.org/10.2307/2682899)</span>

</div>

<div id="ref-bustos-perezFindingOriginalMass2025" class="csl-entry">

<span class="csl-left-margin">73.
</span><span class="csl-right-inline">Bustos-Pérez G. Finding the
original mass: A machine learning model and its deployment for lithic
scrapers. Spinapolice EE, editor. PLOS One. 2025;20: e0327597.
doi:[10.1371/journal.pone.0327597](https://doi.org/10.1371/journal.pone.0327597)</span>

</div>

<div id="ref-lantzMachineLearningExpert2019" class="csl-entry">

<span class="csl-left-margin">74.
</span><span class="csl-right-inline">Lantz B. Machine learning with R:
Expert techniques for predictive modeling. Birmingham: Packt publishing
ltd; 2019. </span>

</div>

<div id="ref-demyttenaereMeanAbsolutePercentage2016" class="csl-entry">

<span class="csl-left-margin">75.
</span><span class="csl-right-inline">De Myttenaere A, Golden B, Le
Grand B, Rossi F. Mean Absolute Percentage Error for regression models.
Neurocomputing. 2016;192: 38–48.
doi:[10.1016/j.neucom.2015.12.114](https://doi.org/10.1016/j.neucom.2015.12.114)</span>

</div>

<div id="ref-rcoreteamLanguageEnvironmentStatistical2019"
class="csl-entry">

<span class="csl-left-margin">76.
</span><span class="csl-right-inline">Team RC. R: A language and
environment for statistical computing. Vienna, Austria: R Foundation for
Statistical Computing; 2019. </span>

</div>

<div id="ref-rstudioteamRStudioIntegratedDevelopment2019"
class="csl-entry">

<span class="csl-left-margin">77.
</span><span class="csl-right-inline">Team Rs. RStudio: Integrated
Development for R. Boston, MA: RStudio, Inc.; 2019. </span>

</div>

<div id="ref-wickhamWelcomeTidyverse2019" class="csl-entry">

<span class="csl-left-margin">78.
</span><span class="csl-right-inline">Wickham H, Averick M, Bryan J,
Chang W, McGowan L, François R, et al. Welcome to the Tidyverse. Journal
of Open Source Software. 2019;4: 1686.
doi:[10.21105/joss.01686](https://doi.org/10.21105/joss.01686)</span>

</div>

<div id="ref-kuhnBuildingPredictiveModels2008" class="csl-entry">

<span class="csl-left-margin">79.
</span><span class="csl-right-inline">Kuhn M. Building Predictive Models
in R using the caret Package. Journal of Statistical Software. 2008;28:
1–26.
doi:[10.18637/jss.v028.i05](https://doi.org/10.18637/jss.v028.i05)</span>

</div>

<div id="ref-maloneyExperimentalArchaeologicalTesting2020a"
class="csl-entry">

<span class="csl-left-margin">80.
</span><span class="csl-right-inline">Maloney TR. Experimental and
archaeological testing with 3D laser scanning reveals the limits of
I/TMC as a reduction index for global scraper and point studies. Journal
of Archaeological Science: Reports. 2020;29: 102068.
doi:[10.1016/j.jasrep.2019.102068](https://doi.org/10.1016/j.jasrep.2019.102068)</span>

</div>

<div id="ref-shottUseMultifactorialReconciliation2017"
class="csl-entry">

<span class="csl-left-margin">81.
</span><span class="csl-right-inline">Shott MJ, Seeman MF. Use and
Multifactorial Reconciliation of Uniface Reduction Measures: A Pilot
Study at the Nobles Pond Paleoindian Site. American Antiquity. 2017;82:
723–741.
doi:[10.1017/aaq.2017.40](https://doi.org/10.1017/aaq.2017.40)</span>

</div>

<div id="ref-boedaLevalloisVolumetricConstruction1995"
class="csl-entry">

<span class="csl-left-margin">82.
</span><span class="csl-right-inline">Boëda E. Levallois: A Volumetric
Construction, Methods, A Technique. In: Dibble HL, Bar-Yosef O, editors.
The Definition and Interpretation of Levallois Technology. Madison,
Wisconsin: Prehistory Press; 1995. pp. 41–68. </span>

</div>

<div id="ref-lycettLevalloisEconomicsExamination2013" class="csl-entry">

<span class="csl-left-margin">83.
</span><span class="csl-right-inline">Lycett SJ, Eren MI. Levallois
economics: An examination of “waste” production in experimentally
produced Levallois reduction sequences. Journal of Archaeological
Science. 2013;40: 2348–2392.
doi:[10.1016/j.jas.2013.01.016](https://doi.org/10.1016/j.jas.2013.01.016)</span>

</div>

</div>
