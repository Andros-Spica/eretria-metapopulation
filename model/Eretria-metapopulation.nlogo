;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GNU GENERAL PUBLIC LICENSE ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;  Household Demography v1.2
;;  Copyright (C) 2021 Andreas Angourakis (andros.spica@gmail.com)
;;  last update Nov 2021
;;  available at https://www.github.com/Andros-Spica/indus-village-model
;;
;;  This program is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;
;;  This program is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.
;;
;;  You should have received a copy of the GNU General Public License
;;  along with this program.  If not, see <http://www.gnu.org/licenses/>.

extensions [ gis ]

;;;;;;;;;;;;;;;;;
;;;;; BREEDS ;;;;
;;;;;;;;;;;;;;;;;

breed [ sites site ]

breed [ households household ]

;;;;;;;;;;;;;;;;;
;;; VARIABLES ;;;
;;;;;;;;;;;;;;;;;

globals
[
  ;;; constants
  maturity-age                     ; defaults to 15 years old; it affects the minimum age acceptable for individuals to keep a household without older individuals
  density-effect-steepness
  patch-x-pixel-scale
  no-data-tag

  ;;; demography tables
  fertility-table
  nuptiality-table-women nuptiality-table-men
  mortality-table-women mortality-table-men

  ;;; GIS dependent
  width
  height
  patch-width      ;;; in m2
  area-per-patch   ;;; in ha
  elevation-data
  patches-with-elevation-data
  min-elevation
  max-elevation
  ;maxDistRivers
  productivity-data
  max-productivity
  min-carrying-capacity-contribution-per-ha
  max-carrying-capacity-contribution-per-ha
  agricultural-land-data
  forest-land-data
  grain-land-data
  rivers-data
  ;distRivers-data
  sites-data
  land-use-list
  max-carrying-capacity-contribution-per-ha-list
  labour-requirement-list

  ;;; modified parameters
  initialNumHouseholdsPerSite
  householdInitialAgeDistribution ; (list minimum maximum)
  maxCoupleCountDistribution      ; (list minimum maximum)
  acceptableKinshipDegreeForCouples ; degree of kinship acceptable between two individuals forming a new couple (1 = same household, 0.5 = level one relationship, etc.)

  densityEffectScalingFactor     ; the maximum additional death probability (scaling factor for density-dependent mortality)

  ;;; variables
  ;;;; auxiliar
  catchment-radius

  ; these are lists of string-coded individuals
  womenToMarry                   ; single individuals selected to marry ("<household who> <member index>")
  menToMarry
  orphanList                     ; orphan children left without adults ("<sex> <age>")

  ;;;; counters and final measures
  totalHouseholds                ; count households (integer)
  totalIndividuals               ; sum of members of households (integer)
  totalPopulationGrowth          ; % of last totalIndividuals
  totalWomen                     ; sum of female members of households (integer)
  totalMen                       ; sum of male members of households (integer)
  femaleRatio                    ; total number of women over total number of individuals (float)
  womenAgeStructure              ; merge list of all households female members ages (list of integers)
  menAgeStructure                ; merge list of all households male members ages (list of integers)
  womenFirstAgeGroup             ; count of women with age from 0 to 4 years old (base of typical population pyramid) (integer)
  menFirstAgeGroup               ; count of men with age from 0 to 4 years old (base of typical population pyramid) (integer)
  womenBirths                    ; number of births (integer)
  menBirths
  womenDeaths                    ; number of deaths (integer)
  menDeaths
  womenIn                        ; number of individuals entering the system (generated for couple creation with external population) (integer)
  menIn
  womenOut                       ; number of individuals exiting the system (due to couple creation with external population) (integer)
  menOut
  totalOrphans                   ; number of children moving from an household where all adults are dead
]

;;; patches variables

patches-own
[
  elevation
  isRiver
  distance-to-rivers
  land-use
  productivity
  carrying-capacity-contribution-per-ha
  labour-demand
  assigned-site
  in-use
]

;;; agent variables

sites-own
[
  name
  siteType
  territory
  carrying-capacity              ; carrying capacity or maximum number of people that can be sustained, i.e. mortality is increased in proportion the population relation to it (density effect)
  coastal-bonus
  my-households
  population
  labour-force
  population-pressure
]

households-own
[
  hh_lineage                     ; identifier of the lineage history of a household (list of integers; integers are household 'who's)
  hh_age                         ; number of years during which the household existed (integer)
  hh_maxCoupleCount              ; max. number of couples accepted within a household (integer)
  hh_membersAge                  ; ages of every household member (list of integer)
  hh_membersSex                  ; sex of every household member (list of true/false, i.e. is female?)
  hh_membersMarriage             ; couple index of every member (list of integers; 0-Inf: couple index, -1: member is single)

  ;;; auxiliar
  hh_memberIndexesToDelete
]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SETUP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to setup

  clear-all

  set-constants

  set-parameters

  create-map

  build-demography-tables

  setup-households

  update-sites

  reset-counters

  update-counters

  update-display

  reset-ticks

end

to set-constants

  ; parameter set to default value (constant)
  set maturity-age 15

  set density-effect-steepness 3

  set patch-x-pixel-scale 1

  set no-data-tag -9999

end

to set-parameters

  ; set random seed
  random-seed SEED

  ; check parameters values
  parameters-check1

  ;;; setup parameters depending on the type of experiment
  set householdInitialAgeDistribution read-from-string ( word "[" household-initial-age-distribution "]")
  set maxCoupleCountDistribution read-from-string ( word "[" max-couple-count-distribution "]")

  if (type-of-experiment = "user-defined")
  [
    ;;; load parameters from user interface
    set initialNumHouseholdsPerSite initial-num-households-per-site

    set acceptableKinshipDegreeForCouples acceptable-kinship-degree-for-couples

    set densityEffectScalingFactor density-effect-scaling-factor
  ]
  if (type-of-experiment = "random")
  [
    ;;; use values from user interface as a maximum for random uniform distributions
    set initialNumHouseholdsPerSite 1 + random initial-num-households-per-site

    set householdInitialAgeDistribution (list
      (1 + random (item 0 householdInitialAgeDistribution))   ; minimum
      (
        (item 0 householdInitialAgeDistribution)
        + 1
        + random ((item 1 householdInitialAgeDistribution) - (item 0 householdInitialAgeDistribution))
      )   ; maximum
      )
    set maxCoupleCountDistribution (list
      (1 + random (item 0 maxCoupleCountDistribution))   ; minimum
      (
        (item 0 maxCoupleCountDistribution)
        + 1
        + random ((item 1 maxCoupleCountDistribution) - (item 0 maxCoupleCountDistribution))
      )   ; maximum
      )

    set cdmlt-level 1 + random 25
    set coale-demeny-region item random 4 (list "west" "east" "south" "north")
    set c1-fert max (list 1E-6 (random-float 1))
    set mu-fert maturity-age + (random 40 - maturity-age)
    set sigma1-fert max (list 1E-6 (random-float (mu-fert - maturity-age)))
    set sigma2-fert max (list 1E-6 (random-float ((30 + maturity-age) - mu-fert)))
    set residence-rule item random 3 (list "patrilocal-patrilineal" "matrilocal-matrilineal" "random")
    set acceptableKinshipDegreeForCouples random 10
    set c1-women max (list 1E-6 (random-float 1))
    set c1-men max (list 1E-6 (random-float 1))
    set mu-women random 40
    set mu-men random 40
    set sigma1-women max (list 1E-6 (random-float mu-women))
    set sigma1-men max (list 1E-6 (random-float mu-men))
    set sigma2-women max (list 1E-6 (random-float (40 - mu-women)))
    set sigma2-men max (list 1E-6 (random-float (40 - mu-men)))

    ;set carryingCapacity 100 + random-float 900
    set densityEffectScalingFactor 0.05 + random-float 0.1
  ]

  set max-carrying-capacity-contribution-per-ha-list (list max-carrying-capacity-contribution-per-ha-grain max-carrying-capacity-contribution-per-ha-other max-carrying-capacity-contribution-per-ha-forest)

  set labour-requirement-list (list labour-requirement-per-ha-grain labour-requirement-per-ha-other labour-requirement-per-ha-forest)

  ; check parameters values
  parameters-check2

end

to parameters-check1

  ;;; check if values were reset to 0
  ;;; and set default values
  if (initial-num-households-per-site = 0)      [ set initial-num-households-per-site               25 ]

  if (cdmlt-level = 0)                          [ set cdmlt-level                           8 ]
  if (c1-fert = 0)                              [ set c1-fert                               0.9 ]
  if (c1-women = 0)                             [ set c1-women                              0.9 ]
  if (c1-men = 0)                               [ set c1-men                                0.85 ]
  if (mu-fert = 0)                              [ set mu-fert                              15 ]
  if (mu-women = 0)                             [ set mu-women                             15 ]
  if (mu-men = 0)                               [ set mu-men                               20 ]
  if (sigma1-fert = 0)                          [ set sigma1-fert                           5 ]
  if (sigma1-women = 0)                         [ set sigma1-women                          5 ]
  if (sigma1-men = 0)                           [ set sigma1-men                            2 ]
  if (sigma2-fert = 0)                          [ set sigma2-fert                           2 ]
  if (sigma2-women = 0)                         [ set sigma2-women                          2 ]
  if (sigma2-men = 0)                           [ set sigma2-men                           10 ]

  if (density-effect-scaling-factor = 0)        [ set density-effect-scaling-factor         0.05 ]

  ;;; string type inputs (vector of values)
  if (household-initial-age-distribution = 0 or
    length household-initial-age-distribution = 1)   [ set household-initial-age-distribution   "0 30" ]
  if (max-couple-count-distribution = 0 or
    length max-couple-count-distribution = 1)        [ set max-couple-count-distribution        "1 6" ]

end

to parameters-to-default

  ;;; set parameters to a default value
  set max-iterations                     2000
  set max-population                   100000

  set initial-num-households-per-site      25

  set cdmlt-level                          16
  set coale-demeny-region                "south"

  set c1-fert                               0.95
  set mu-fert                              17
  set sigma1-fert                          3
  set sigma2-fert                          15

  set c1-women                              0.95
  set mu-women                             20
  set sigma2-women                         10
  set sigma1-women                          2

  set c1-men                                0.95
  set mu-men                               20
  set sigma1-men                            2
  set sigma2-men                           10

  set household-initial-age-distribution   "0 30"
  set max-couple-count-distribution        "1 6"

  set density-effect-scaling-factor         0.05

end

to parameters-check2

  ;;; check if given min values are less than max values
  check-par-is-range "householdInitialAgeDistribution" householdInitialAgeDistribution
  check-par-is-range "maxCoupleCountDistribution" maxCoupleCountDistribution

end

to check-par-is-positive [ parName parValue ]

  if (parValue <= 0)
  [
    print (word "ERROR: " parName " must be greater than zero")
    stop
  ]

end

to check-par-is-range [ parName parListMinMax ]

  if ( item 0 parListMinMax > item 1 parListMinMax)
  [
    print (word "ERROR: " parName " minimum (first item) must be less than or equal to maximum (second item)")
    stop
  ]

end

to create-map

  load-gis  ;; load in the GIS data

  set-world-dimensions

  setup-patches ;; use GIS data to set patch variables

  setup-sites ;; create site agents with properties from sites-data

end

to set-world-dimensions

  set width floor ((gis:width-of elevation-data) * patch-x-pixel-scale)
  set height floor ((gis:height-of elevation-data) * patch-x-pixel-scale)

  resize-world 0 (width - 1) 0 (height - 1)

  set-patch-size 5 / patch-x-pixel-scale

  ; 2. Match NetLogo world to the dataset envelope
;  gis:set-world-envelope gis:envelope-of elevation-data
  gis:set-world-envelope (gis:envelope-union-of gis:envelope-of elevation-data gis:envelope-of agricultural-land-data)

  ; Calculate area equivalent to a patch

  ; if units are in degrees
  set area-per-patch calculate-patch-area-degrees elevation-data

  ; if units are in meters
  ;set area-per-patch calculate-patch-area-meters agricultural-land-data

  set patch-width sqrt (area-per-patch * 10000) ;;; m = ha * 10,000

end

to-report calculate-patch-area-degrees [ rasterData ]

  ; Get the envelope: [min-x min-y max-x max-y] in degrees
  let env gis:envelope-of rasterData
  let min-lon item 0 env
  let min-lat item 2 env
  let max-lon item 1 env
  let max-lat item 3 env

  ; Calculate central latitude (φ₀)
  let lat0 (min-lat + max-lat) / 2

  ; Get raster resolution (number of cells)
  set width gis:width-of rasterData     ; number of columns
  set height gis:height-of rasterData   ; number of rows

  ; Cell size (in degrees)
  let dlon (max-lon - min-lon) / width
  let dlat (max-lat - min-lat) / height

  ; Earth radius (meters)
  let R 6371000

  ; Area per raster cell in square meters (approximate)
  let patchArea (R ^ 2) * abs dlon * abs dlat * cos(lat0 * pi / 180) * (pi / 180) ^ 2

  ; Report the result
;  print (word "Approximate area per patch: " patchArea " m² (" (patchArea / 10000) " ha) at latitude " lat0)

  report patchArea / 10000

end

to-report calculate-patch-area-meters [ gisData ]

  let env gis:envelope-of gisData

  let gis-width item 1 env - item 0 env
  let gis-height item 3 env - item 2 env
;  print (word "width: " gis-width ", height: " gis-height)

  let num-patches-x max-pxcor - min-pxcor + 1
  let num-patches-y max-pycor - min-pycor + 1
  ;set patch-width gis-width / num-patches-x

  let patchArea (gis-width * gis-height) / (num-patches-x * num-patches-y)
;  print (word "area-per-patch=" area-per-patch)
  set patchArea patchArea / 10000 ; square meters to hectares

  report patchArea

end

to setup-patches

  setup-elevation

  ask patches [ set land-use item 3 land-use-list ]

;  let agricultural-patches patches gis:intersecting agricultural-land-data
;  ask (agricultural-patches)
;  [
;    set land-use "Agricultural areas"
;  ]

  let forest-patches-eretria patches gis:intersecting forest-land-data
  ask (forest-patches-eretria)
  [
    set land-use item 2 land-use-list
  ]

  let agricultural-patches-eretria patches gis:intersecting agricultural-land-data
  ask (agricultural-patches-eretria)
  [
    set land-use item 1 land-use-list
  ]

  let grain-patches-eretria patches gis:intersecting grain-land-data
  ask (grain-patches-eretria)
  [
    set land-use item 0 land-use-list
  ]

  setup-productivity

  setup-rivers

  setup-land-carrying-capacity-per-ha

  setup-labour-demand

end

to setup-elevation

  ; Resample the raster to match world size
  let resampled-data gis:resample elevation-data gis:world-envelope world-width world-height

  gis:apply-raster resampled-data elevation

  set patches-with-elevation-data patches with [(elevation <= 0) or (elevation >= 0)]

  ;;; replace NaN values added by the gis extension with no-data-tag, so it does not generate problems after
  ask patches with [not ((elevation <= 0) or (elevation >= 0))] [ set elevation no-data-tag ]

  set max-elevation max [elevation] of patches-with-elevation-data
  set min-elevation min [elevation] of patches-with-elevation-data

end

to setup-productivity

  ; Resample the raster to match world size
  let resampled-data gis:resample productivity-data gis:world-envelope world-width world-height

  gis:apply-raster resampled-data productivity

  ;;; replace NaN values added by the gis extension with no-data-tag, so it does not generate problems after
  ask patches with [land-use = item 3 land-use-list] [ set productivity no-data-tag ]

  set max-productivity max [productivity] of patches with [land-use != item 3 land-use-list]

end

to setup-rivers

  ;print gis:feature-list-of rivers-data
  ask patches
  [
    set isRiver gis:intersects? rivers-data self
  ]

  ; Resample the raster to match world size
  ;let resampled-data gis:resample distRivers-data gis:world-envelope world-width world-height

  ;gis:apply-raster resampled-data distance-to-rivers

  ;set maxDistRivers max [distance-to-rivers] of patches with [land-use != item 3 land-use-list]

end

to setup-land-carrying-capacity-per-ha

  ask patches with [land-use != item 3 land-use-list]
  [
    ;;; weight carrying capacity per ha of land use, factoring extra productivity factors: elevation, slope, distance-to-rivers
    set carrying-capacity-contribution-per-ha (get-carrying-capacity-contribution-per-ha land-use) * productivity
  ]

  ask patches with [land-use = item 3 land-use-list] [ set carrying-capacity-contribution-per-ha no-data-tag ]

  set min-carrying-capacity-contribution-per-ha min [carrying-capacity-contribution-per-ha] of patches with [land-use != item 3 land-use-list]
  set max-carrying-capacity-contribution-per-ha max [carrying-capacity-contribution-per-ha] of patches with [land-use != item 3 land-use-list]

end

to setup-labour-demand

  ask patches with [land-use != item 3 land-use-list]
  [
    set labour-demand (get-labour-requirement-per-ha land-use) * area-per-patch
  ]

end

to setup-sites

  set catchment-radius floor max-catchment-distance * 1000 / patch-width

  foreach gis:feature-list-of sites-data
  [
    vectorFeature ->

    let coordTuple gis:location-of (first (first (gis:vertex-lists-of vectorFeature)))
    ;let featureName gis:property-value vectorFeature "NAME"
    ;let featureType gis:property-value vectorFeature "TYPE"

    let long item 0 coordTuple
    let lat item 1 coordTuple

    create-sites 1
    [
      setxy long lat
      ;set name featureName
      ;set siteType featureType
      ;set period datasetPeriod

      site_initialise

      set shape "circle 2"
    ]
  ]

;  ask patch (round (0.28 * world-width)) (round (0.5 * world-height))
;  [
;    sprout-sites 1
;    [
;      set name "Eretria"
;      set siteType "settlement"
;
;      set territory patches in-radius site_get-catchment
;      set carrying-capacity sum [carrying-capacity-contribution-per-ha] of territory ; with [any? households-here]
;
;      set shape "house"
;      set size 6
;    ]
;  ]

end

to setup-households

  ask sites
  [
    hatch-households initialNumHouseholdsPerSite
    [
      hh_initialise

      set hidden? true
      ;set shape "person"
      ;set size 6
    ]

    set my-households households-here
  ]

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GO ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to go

  reset-counters

  update-households

  update-sites

  update-counters

  display-sites

  if (totalHouseholds = 0 or
    ticks = max-iterations or
    totalIndividuals > max-population)
  [
    ;if (length behaviorspace-experiment-name > 0 and count households > 0) [ export-households ]
    stop
  ]
  ;print "-------tick----"
  tick

end

;;; GLOBAL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to update-households

  ;print "-------apply-mortality----"
  age-households

  ;print "-------apply-mortality----"
  apply-mortality

  ;print "-------apply-nuptiality----"
  apply-nuptiality

  ;print "-------apply-fertility----"
  apply-fertility

  ;print "-------manage-orphanhood----"
  manage-orphanhood

  ;print "-------solve-migrations----"
  solve-migrations

end

to age-households

  ask households
  [
    hh_aging
  ]

end

to apply-mortality

  ask households
  [
    hh_update-members-survival
  ]

end

to apply-nuptiality

  build-lists-to-marry

  try-to-form-couples

  clear-former-memberships

end

to build-lists-to-marry

  ; initialize population lists of marrying candidates for this year
  set womenToMarry (list)
  set menToMarry (list)

  ; fill lists (random order)
  ask households
  [
    ; add to the womenToMarry/menToMarry any women/men that should be marrying, accordint to the nuptiality model
    hh_set-members-to-marry
  ]

  ;print (word "womenToMarry: " womenToMarry)
  ;print (word "menToMarry: " menToMarry)

end

to try-to-form-couples

  ; iterate for every women and men to marry until all form couples
  ; or are included in the womenWithoutMatch/menWithoutMatch lists

  ; iterate for every women to marry
  foreach n-values (length womenToMarry) [i -> i]
  [
    womanIndex ->

    ; initialise this women as single (null husband index in menToMarry)
    let husbandIndex -1

    ; iterate for every men still to marry (this loop is skipped if there is no menToMarry)
    foreach n-values (length menToMarry) [i -> i]
    [
      manIndex ->

      ; try to create couple if still did not found husband
      if (husbandIndex = -1)
      [
        ; create-couple returns true only if the procedure could form the couple
        if (create-couple (item womanIndex womenToMarry) (item manIndex menToMarry))
        [
          set husbandIndex manIndex
        ]
      ]
    ]

    ifelse (husbandIndex = -1)
    [
      ; if no husband was found within this population, create the new couple with this woman and an external man
      create-couple-external (item womanIndex womenToMarry)
    ]
    [
      ; if husband was found within this population, remove him from menToMarry list
      set menToMarry remove-item husbandIndex menToMarry
    ]
  ]

  ; iterate for every men still to marry, creating new couples with external women
  foreach n-values (length menToMarry) [i -> i]
  [
    manIndex ->
    create-couple-external (item manIndex menToMarry)
  ]

end

to clear-former-memberships

  ; delete recently married individuals from their parent household
  ask households
  [
    hh_delete-members-in-queue
  ]

end

to-report create-couple [ womanData manData ]

  let womanHousehold item 0 womanData
  let womanIndex item 1 womanData
  let manHousehold item 0 manData
  let manIndex item 1 manData

;  print (word "index " womanIndex " in " [hh_membersAge] of womanHousehold ", female, " womanHousehold)
;  print (word "index " manIndex " in " [hh_membersAge] of manHousehold ", male, " manHousehold)

  ; first, test if new couple vulnerates any tabu
  ifelse (couple-is-acceptable womanHousehold manHousehold)
  [
    if (residence-rule = "patrilocal-patrilineal")
    [
      ask manHousehold
      [
        hh_try-to-add-couple manIndex womanData
      ]
    ]
    if (residence-rule = "matrilocal-matrilineal")
    [
      ask womanHousehold
      [
        hh_try-to-add-couple womanIndex manData
      ]
    ]
    if (residence-rule = "random")
    [
      ifelse (random 2 > 0)
      [
        ask manHousehold
        [
          hh_try-to-add-couple manIndex womanData
        ]
      ]
      [
        ask womanHousehold
        [
          hh_try-to-add-couple womanIndex manData
        ]
      ]
    ]
    ;;; Other residence rules can be added here
;    print "couple is acceptable."

    report true
  ]
  [
;    print "couple is not acceptable."

    report false
  ]

end

to-report couple-is-acceptable [ womanHousehold manHousehold ]

  ; two options:
  ; 1. not at the same household,
  ; 2. not of the same lineage

  ; case 1 (simple): woman and man are in the same household
  ;if (womanHousehold = manHousehold) [ report false ]

  ; case 2 (complex, more flexible/general, includes case 1):
  ; woman and man share the same lineage (greater risk of inbreeding)
  report (get-kinship-degree womanHousehold manHousehold > acceptableKinshipDegreeForCouples)

end

to-report get-kinship-degree [ household1 household2 ]

  ; get the lineage IDs differentiating between longer and shorter (if they have, in fact, a different length)
  let lineage1 [hh_lineage] of household1
  let lineage2 [hh_lineage] of household2

;  print (word household1 " has lineage " lineage1)
;  print (word household2 " has lineage " lineage2)

  let numberOfCommonElements 0

  ; iterate for every element in the shorter lineage ID, until finds a different element or ends the the sequence
  foreach n-values (min (list (length lineage1) (length lineage2))) [j -> j]
  [
    i ->
    ; if still did not found a different element (i.e. numberOfCommonElements < i)
    ; and the elements match
    if (numberOfCommonElements = i and (item i lineage1 = item i lineage2))
    [
      set numberOfCommonElements numberOfCommonElements + 1
    ]
  ]

  ; get the total number of different elements in lineage IDs (i.e., number of household splits separating household1 and household2)
  let numberOfDifferentElements sum (list (length lineage1 - numberOfCommonElements) (length lineage1 - numberOfCommonElements))

  ; case examples:
  ; between [1 2 1 5] and  [1 2 3 7],  it is 4 (diff. elements are 1 5 and 3 7)
  ; between [15 3 1 2] and [15 4 1 2], it is 6 (diff. elements are 3 1 2 and 4 1 2)
  ; between [6 3 8] and    [6 3],      it is 1 (diff. element is 8). Household [6 3 8] was formed with a member of the household [6 3]

;  print (word "kinship degree: " numberOfDifferentElements)

  report numberOfDifferentElements

end

to create-couple-external [ singleData ]

  let singleHousehold item 0 singleData
  let singleIndex item 1 singleData
  let singleSex [item singleIndex hh_membersSex] of singleHousehold

;  print (word "Member " singleIndex " in " singleHousehold " (ages = " [hh_membersAge] of singleHousehold ", sex = " [hh_membersSex] of singleHousehold ") is marring outside the system."  )

  ; find out if the new couple imply a new individual to be entering the system
  let newIndividualIn
  (
    ; a new individual comes in either when...
    ; ...the single is a woman and the residence rule is matrilocal (husband comes in)
    (singleSex and residence-rule = "matrilocal-matrilineal")
    or
    ; ...the single is a men and the residence rule is patrilocal (wife comes in)
    ((not singleSex) and residence-rule = "patrilocal-patrilineal")
    or
    ; ...the residence rule is random
    (residence-rule = "random")
  )

  ifelse (newIndividualIn)
  [
    ; a new individual is entering the system...

    ; try to add a new couple in singleHousehold
    ask singleHousehold
    [
      hh_try-to-add-couple singleIndex [ -1 -1 ] ; pass spouse data as a NULL value (messaging it should be generated)
    ]

    ; account for the new individual coming in
    ifelse (not singleSex)
    [
      set womenIn womenIn + 1 ; if single is male, the new individual is female
    ]
    [
      set menIn menIn + 1
    ]
  ]
  [
    ; the single individual is the one moving, so exiting the system...

;    print (word "individual from " singleHousehold " moving out the system: is female = " singleSex ", age = " (item singleIndex [hh_membersAge] of singleHousehold) )

    ; add the single individual (and any children left alone) to the deletion queue of singleHousehold
    let childrenMovingOut 0
    ask singleHousehold
    [
;      ifelse (hh_is-infants-only)
;      [
;        ;print (word "all remaining individuals in " singleHousehold " are children: ages including single adult = " hh_membersAge )
;        ; add all members to deletion queue since the household would have only children (assuming they are moving out with the single individual)
;        set hh_memberIndexesToDelete hh_membersIndexes
;
;        set childrenMovingOut hh_count-children
;      ]
;      [
        ; add the single individual to the deletion queue, avoiding duplicates
        set hh_memberIndexesToDelete remove-duplicates (lput singleIndex hh_memberIndexesToDelete)
;      ]
    ]

    ; account for the single individual going out
    ifelse (singleSex)
    [
      set womenOut womenOut + 1 + childrenMovingOut
    ]
    [
      set menOut menOut + 1 + childrenMovingOut
    ]
  ]

end

to apply-fertility

  ask households
  [
    hh_reproduce
  ]

end

to manage-orphanhood

  ; initialize population lists of orphan children for this year
  set orphanList (list)

  ; go through every household checking if there is at least one adult and, if not,
  ; placing all infants in the orphan list and erasing them from the household until eventually the household is disolved
  ask households
  [
    hh_disolve-if-no-adults
  ]

  if (any? households) ; do it only in case there is any household left
  [
    ; distribute orphans randomly among surviving households
    foreach n-values length orphanList [ j -> j ]
    [
      i ->

      ask one-of households
      [
        hh_add-orphan (item i orphanList)
      ]
    ]
  ]

end

to solve-migrations

  ask sites with [population-pressure > 0.5]
  [
    let this-site self
    let candidates-to-leave n-of (floor (( clamp01 (population-pressure - 0.5)) * (count my-households))) my-households
    ask candidates-to-leave
    [
      let bestSite min-one-of sites [count territory with [in-use > 0]];[population-pressure]
      if (bestSite != this-site) [ hh_migrate bestSite ]
    ]
  ]

end

to update-sites

  ask sites
  [
    site_update
  ]

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SITES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to site_initialise

  site_set-territory

  site_set-coastal-bonus

end

to site_update

  set my-households households-here

  set population site_count-population

  set labour-force site_get-total-labour

  site_update-carrying-capacity

  set population-pressure 0
  if (population > 0)
  [
    set population-pressure clamp01 (population / carrying-capacity)
  ]

end

to site_set-territory

  let me self

  set territory (patches with [ land-use != item 3 land-use-list ]) in-radius catchment-radius

  ask territory
  [
    let dist distance myself
    if (assigned-site = 0)
    [
      set assigned-site me
    ]
    if (assigned-site != me and distance myself < distance assigned-site)
    [
      set assigned-site me
    ]
  ]

  set territory patches with [ assigned-site = me ]

end

to site_set-coastal-bonus

  let me self

  set coastal-bonus 0

  let coastal-patches-in-territory (patches in-radius catchment-radius) with [count neighbors with [elevation < 0] > 0]
  ;print coastal-patches-in-territory
  if (count coastal-patches-in-territory > 0)
  [
    ;print min [distance me] of coastal-patches-in-territory
    set coastal-bonus max-coastal-bonus * (1 - (min [distance me] of coastal-patches-in-territory) / catchment-radius)
  ]

end

to site_update-carrying-capacity

  let this-site self
;print this-site

  let labour-available labour-force

  set carrying-capacity 0

  foreach (sort-on [distance this-site] territory)
  [
    aPatch ->

    ifelse (labour-available > 0)
    [
;print (word aPatch ": land-use = " ([land-use] of aPatch))
      let labour-demand-of-aPatch [labour-demand] of aPatch
;print (word "available: " labour-available " vs required: " labour-demand-of-aPatch)
      let laboured-patch-share 1
      if (labour-available < labour-demand-of-aPatch)
      [ set laboured-patch-share labour-available / labour-demand-of-aPatch ]
;print (word "laboured-patch-share = " laboured-patch-share)
;print (word "carrying-capacity-contribution-per-ha = " carrying-capacity-contribution-per-ha)
;print (word "productivity = " productivity)
      ;;; total per patch area that can be laboured
      set carrying-capacity carrying-capacity + carrying-capacity-contribution-per-ha * laboured-patch-share * area-per-patch

      set labour-available max (list 0 (labour-available - labour-demand-of-aPatch))
;print (word "carrying-capacity: " carrying-capacity ", labour-available: " labour-available)
      ask aPatch [ set in-use laboured-patch-share ]
    ]
    [
      ask aPatch [ set in-use 0 ]
    ]
  ]

  ;;; apply coastal-bonus
  set carrying-capacity carrying-capacity * (1 + coastal-bonus / 100)

end

to-report site_get-density-effect

  let carrying-capacity-demand sum [hh_get-total-carrying-capacity-demand] of households-here

  report get-density-effect carrying-capacity-demand carrying-capacity

end

to site_highlight-territory

  let me self
  ask territory
  [
    set pcolor ([color] of me) + 3
  ]

end

to site_highlight-territory-in-use

  let me self
  ask territory with [in-use > 0]
  [
    set pcolor [color] of me - 3 * in-use
  ]

end

to-report site_count-population

  report sum [hh_count-members] of my-households

end

to-report site_get-total-labour

  let labour 0

  ask my-households
  [
    set labour labour + hh_get-total-labour
  ]

  report labour

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HOUSEHOLDS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Initialisation first generation households ==============================================================================
to hh_initialise

  ;;; Initialization of households

  set hh_lineage (list who) ; initialise lineage ID with this household 'who'
  set hh_age get-initial-household-age
  set hh_maxCoupleCount get-initial-max-couple-count
  set hh_memberIndexesToDelete (list)

  hh_initialise-members

end

to-report get-initial-household-age

  report item 0 householdInitialAgeDistribution + random (item 1 householdInitialAgeDistribution - item 0 householdInitialAgeDistribution + 1)

end

to-report get-initial-max-couple-count

  report item 0 maxCoupleCountDistribution + random (item 1 maxCoupleCountDistribution - item 0 maxCoupleCountDistribution + 1)

end

to hh_initialise-members

  hh_reset-members

  ; assures that at least two member are above (fertility age + household age) and have different sex
  foreach (list 0 1)
  [
    ?1 ->
    set hh_membersSex lput (?1 = 0) hh_membersSex
    let marriageAge (get-initial-marriage-age (item ?1 hh_membersSex))
    set hh_membersAge lput (hh_age + marriageAge) hh_membersAge
    set hh_membersMarriage lput 0 hh_membersMarriage ; this couple will have the 0 index
  ]

  ; calculate offspring and offspring age
  let offspringProb 0
  let i hh_age
  repeat hh_age
  [
    ; get the probability of the couple having descendence for a past year i
    ; i.e. woman fertility at the corresponding age
    set offspringProb get-fertility ((item 0 hh_membersAge) - i)

    ; test the probability and add the offspring with the corresponding age
    if (random-float 1 < offspringProb)
    [
      hh_add-offspring i ; a child with i years old
    ]
    set i i - 1
  ]

end

to-report get-initial-marriage-age [ isFemale ]

  ; get a marriage age as a stochastic function of nuptiality rates in earlier years
  let notMarried true
  let age 0

  while [notMarried AND age < 100]
  [
    if ((get-nuptiality isFemale age) > random-float 1)
    [
      set notMarried false
    ]
    set age age + 1
  ]

  report age

end

; Main procedures ==============================================================================
to hh_aging

  ; household aging
  set hh_age hh_age + 1

  ; members aging
  hh_update-members-age

end

to hh_update-members-age

  ; household members aging
  set hh_membersAge map [ i -> i + 1 ] hh_membersAge

end

to hh_update-members-survival

  ; applies age-specific mortality rates and delete dying members

  ; get a list with true/false values flagging which members is dying during the current year
  let dying? hh_whichMembersDying

  ; iterate for the members from last to first, eliminating those that should be dying
  let index (hh_count-members) - 1
  repeat hh_count-members
  [
    if (item index dying?)
    [
      ; add to mortality
      ifelse (item index hh_membersSex)
      [ set womenDeaths womenDeaths + 1 ]
      [ set menDeaths menDeaths + 1 ]

      ;print (word "member from " self " dies with age " (item index hh_membersAge))
      hh_delete-member index
    ]
    set index index - 1
  ]

  hh_update-marriages-after-deaths

end

to-report hh_whichMembersDying

  ;;; density effect is distributed equally among members
  let densityEffect [site_get-density-effect] of one-of sites-here

  ; returns a list with true/false values flagging which members is dying during the current year
  report map
  [
    i ->
    (random-float 1 < get-net-mortality (item i hh_membersSex) (item i hh_membersAge) densityEffect)
  ] hh_membersIndexes

end

to hh_update-marriages-after-deaths

  ; update hh_membersMarriage to consider any widow/widower as single
  foreach hh_membersIndexes
  [
    i ->
    if (item i hh_membersMarriage != -1                                           ; if the member is married
      and length filter [j -> j = item i hh_membersMarriage] hh_membersMarriage = 1) ; and her/his partner died this year (so she/he is the only member with the marriage index)
    [
      set hh_membersMarriage replace-item i hh_membersMarriage -1
      ;print (word "a member of " self " has lost her/his partner.")
    ]
  ]

end

to hh_set-members-to-marry

  ; applies age-specific nuptiality rates

  ; iterate for each member, finding which one is marrying during the current year, according to age cohort and sex
  foreach hh_membersIndexes
  [
    i ->
    if (item i hh_membersMarriage = -1) ; member is not married
    [
      let sex item i hh_membersSex

      if (random-float 1 < get-nuptiality sex (item i hh_membersAge))  ; passes nuptiality test according to age
      [
        ifelse (sex)
        [
          set womenToMarry lput (list self i) womenToMarry
        ]
        [
          set menToMarry lput (list self i) menToMarry
        ]
      ]
    ]
  ]

end

to hh_try-to-add-couple [ selfIndex spouseData ]

  ; check if new couple fits in self's household
  let tooManyCouples (hh_count-couples + 1) > hh_maxCoupleCount

  ifelse (tooManyCouples)
  [
    ; the new couple must found a new household, descending from self's household
    ;print "household fission"
    hh_household-fission selfIndex spouseData
  ]
  [
    ; the new couple can stay in the self's parent household
    hh_add-couple selfIndex spouseData
  ]

end

to hh_household-fission [ selfIndex spouseData ]

  ; creates a new household descending from self household,
  ; adding self and spouse and
  ; deleting them from the respective parent households

  ;print (word self " is fissioning")

  let selfHousehold self

  ; create new household w/ self and spouse
  hatch 1
  [
    ; expand lineage ID by adding this household 'who' to the lineage ID of the parent household
    set hh_lineage lput who [hh_lineage] of selfHousehold
    ; !!!

    set hh_age 0
    ; inherit hh_maxCoupleCount value from parent household

    hh_reset-members

    ; add self
    hh_add-member-from (list selfHousehold selfIndex)
    ; add spouse
    hh_add-spouse 0 spouseData ; 0 is the index of the self individual in the new household

    ; account for the new couple
    set hh_membersMarriage lput 0 hh_membersMarriage ; 0 because this is the first couple of the new household
    set hh_membersMarriage lput 0 hh_membersMarriage

    ; initialise list of members to delete (used during apply-nuptiality)
    set hh_memberIndexesToDelete (list)
  ]

end

to hh_add-couple [ selfIndex spouseData ]

  ; add spouse to self's household
  ;print (word "adding couple to " self)

  hh_add-spouse selfIndex spouseData

  ; account for the new couple
  let newCoupleIndex 1 + max hh_membersMarriage ; create a new couple index
  set hh_membersMarriage replace-item selfIndex hh_membersMarriage newCoupleIndex ; update self's marriage status
  set hh_membersMarriage lput newCoupleIndex hh_membersMarriage ; add another corresponding to the spouse

end

to hh_reproduce

  ; iterate for the members, up to the number of couples,
  ; testing women for the corresponding fertility rate
  ; and generates a new born individual if passing test.

  let couplesToTest hh_count-couples

  foreach hh_membersIndexes
  [
    i ->
    ; there is still a couple to consider and the member is female
    if (couplesToTest > 0 and item i hh_membersSex)
    [
      if (random-float 1 < get-fertility (item i hh_membersAge))
      [
        ;print get-fertility (item i hh_membersAge)
        hh_add-offspring 0 ; add a newborn
        ;print (word "a new member is born in " self)
      ]
      set couplesToTest couplesToTest - 1
    ]
  ]

end

to hh_disolve-if-no-adults

  ; check if the household is left without any adult
  if (hh_is-infants-only)
  [
    ; there is no adult in this household, so the children will enter the orphanList so they can be adopted later
    ;print (word "all remaining individuals in " self " are children: is female = " hh_membersSex ", ages = " hh_membersAge ". They are now in the orphan list.")

    ; iterate for the members from last to first
    let index (hh_count-members) - 1
    repeat hh_count-members
    [
      ; add children data (sex, age) to the orphan list (they are distributed among other households once aging procedures are done)
      set orphanList lput (list (item index hh_membersSex) (item index hh_membersAge) ) orphanList

      ; and delete them from the current household (eventualy erasing it)
      hh_delete-member index

      set index index - 1
    ]
  ]

end

to hh_add-spouse [ selfIndex spouseData ]

  ; if spouseData is NULL -> [ -1 -1 ]
  ifelse (item 0 spouseData = -1)
  [
    ; The spouse is entering the system:
    ; generate and add spouse's sex and age
    set hh_membersSex lput (not item selfIndex hh_membersSex) hh_membersSex ; opposite sex from selfIndex's

    ;print (word "new spouse added to " self ": is female = " (last hh_membersSex) ", age = " (last hh_membersAge))
  ]
  [
    ; The spouse is already in the system:
    ; copy spouse
    hh_add-member-from spouseData

    ;print (word "spouse moving from " (Household (item 1 spouseData)) " to " self ": is female = " (last hh_membersSex) ", age = " (last hh_membersAge))
  ]

end

to hh_add-member-from [ memberData ]

  let aHousehold item 0 memberData
  let index item 1 memberData

  set hh_membersAge lput item index ([hh_membersAge] of aHousehold) hh_membersAge
  set hh_membersSex lput item index ([hh_membersSex] of aHousehold) hh_membersSex

  ; add member to deletion queue of original parent household
  ask aHousehold
  [
    set hh_memberIndexesToDelete lput index hh_memberIndexesToDelete
  ]

end

to hh_add-offspring [ initialAge ]

  ; add a newborn to the household
  ; the specification of initialAge is needed to use this in setup
  set hh_membersAge lput initialAge hh_membersAge
  set hh_membersSex lput (random 2 = 0) hh_membersSex
  set hh_membersMarriage lput -1 hh_membersMarriage ; any offspring will be single

  ifelse (last hh_membersSex)
  [ set womenBirths womenBirths + 1 ]
  [ set menBirths menBirths + 1 ]

end

to hh_add-orphan [ orphanData ]

  ; add an orphan child to the household
  ; orphanData is assumed to be: [ <sex> <age> ]

  set hh_membersSex lput (item 0 orphanData) hh_membersSex
  set hh_membersAge lput (item 1 orphanData) hh_membersAge
  set hh_membersMarriage lput -1 hh_membersMarriage ; any orphan is assumed single
  ;(when children "marry", they will share the household with the spouse. If they are found an orphan is because the spouse is either dead or is also an orphan)

  ;print (word "the orphan (is female = " (item 0 orphanData) ", age = " (item 1 orphanData) ") is adopted by " self ": are females = " hh_membersSex ", ages = " hh_membersAge )

end

to hh_delete-members-in-queue

  ;print self
  ;print hh_membersAge
  ;print hh_memberIndexesToDelete
  ; delete members in queue following decreasing order (so indexes still to go remain valid)
  foreach sort-by > (remove-duplicates hh_memberIndexesToDelete)
  [
    i ->
    ; delete member from this household
    hh_delete-member i
  ]

  ; reset queue
  set hh_memberIndexesToDelete (list)

end

to hh_delete-member [ index ]

  set hh_membersAge remove-item index hh_membersAge
  set hh_membersSex remove-item index hh_membersSex
  set hh_membersMarriage remove-item index hh_membersMarriage
  ; if the member was married, it will imply that there will be an odd number of married members,
  ; thus discounting a couple in hh_count-couples

  if (hh_count-members = 0) [ die ] ; delete empty household

end

to-report hh_is-infants-only

  ; filter out the members selected to be deleted
  let membersAgeNotInQueueToDelete hh_membersAge

  foreach sort-by > hh_memberIndexesToDelete
  [
    i ->
    set membersAgeNotInQueueToDelete remove-item i membersAgeNotInQueueToDelete
  ]

  if (length membersAgeNotInQueueToDelete = 0) [ report false ] ; report false in case there is no members that are not in queue to deletion

  report reduce and (map [i -> i < maturity-age] membersAgeNotInQueueToDelete)

end

to-report hh_count-members

  report length hh_membersAge

end

to-report hh_count-children

  report length filter [i -> i < maturity-age] hh_membersAge

end

to-report hh_count-couples

  let marriedMembers filter [ i -> (i >= 0) ] hh_membersMarriage

  report floor ((length marriedMembers) / 2)

end

to-report hh_membersIndexes

  ; report a list of members indexes from 0 to n

  report (n-values hh_count-members [ i -> i ])

end

to-report hh_get-total-carrying-capacity-demand

  let carrying-capacity-demand 0

  foreach hh_membersAge
  [
    i ->
    set carrying-capacity-demand carrying-capacity-demand + (get-carrying-capacity-demand-at-age i)
  ]

  report carrying-capacity-demand

end

to-report hh_get-total-labour

  let labour 0

  foreach hh_membersAge
  [
    i ->
    set labour labour + (get-labour-at-age i)
  ]

  report labour

end

to hh_migrate [ aSite ]

  move-to aSite

  ask aSite [ site_update ]

end

to hh_reset-members

  set hh_membersAge (list)
  set hh_membersSex (list)
  set hh_membersMarriage (list)

  set hh_memberIndexesToDelete (list)

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Demographic rates 'getters' ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report get-fertility [ age ]

  report item age fertility-table

end

to-report get-nuptiality [ isFemale age ]

  ifelse (isFemale)
  [
    report item age nuptiality-table-women
  ]
  [
    report item age nuptiality-table-men
  ]

end

to-report get-mortality [ isFemale age ]

  ifelse (isFemale)
  [
    report item age mortality-table-women
  ]
  [
    report item age mortality-table-men
  ]

end

to-report get-net-mortality [ isFemale age densityEffect ]

  let mortality (get-mortality isFemale age)

  report clamp01 (mortality + densityEffect)

end

to-report get-density-effect [ carryingCapacityDemand carryingCapacity ]

  ; For example, in https://www.wolframalpha.com/, use:
  ;plot 0.05*(x / 200) ^ 3 for x=1..1000
  let densityEffect density-effect-scaling-factor * (carryingCapacityDemand / (carryingCapacity + 1E-6)) ^ density-effect-steepness

  report densityEffect

end

to-report get-carrying-capacity-contribution-per-ha [ landUse ]

  ;;; get maximum contribution per hectare of land use category
  report item (position landUse land-use-list) max-carrying-capacity-contribution-per-ha-list

  ;;; NOTES on carrying capacity values (ChatGPT):
;;; # Assuming that grain crops support up to 4 adult persons per hectare per annum, what could be reasonable equivalent values for the other two land use categories mentioned (other crops/herding and forestry)?

;## 🗒️ Answer
;
;Using the 4 persons/hectare/year value for grain crops as a baseline, reasonable productivity equivalencies in terms of **adult persons supported per hectare per year** could be:
;
;1. **Other crops/herding (e.g., olives, goats, sheep): \~2.5 persons/ha/year**
;
;   * **Olive trees** yield around 45 kg/tree/year, with \~100 trees/ha typical in Mediterranean agroforestry, giving \~4500 kg/ha. Based on caloric value and oil yield, this supports \~1.5–2 persons \[[1](http://gallifreypermaculture.com.au/2016/11/olives-for-integrated-livestock-orchards/), [5](https://www.mdpi.com/2077-0472/13/12/2194)].
;   * **Grazing** (goats/sheep) on Mediterranean scrub or integrated systems may add enough meat/milk to bring this to **\~2.5 persons/ha/year** in well-managed systems \[[7](https://www.researchgate.net/publication/234092558_Diet_selection_and_performance_of_sheep_and_goats_grazing_on_different_heathland_vegetation_types)].
;
;2. **Forestry: \~0.2–0.5 persons/ha/year**
;
;   * Low value per area due to long growth cycles and limited annual harvest (timber, resin, biomass).
;   * Some multifunctional uses (e.g., mushrooms, wild herbs, grazing under pine) can push this up slightly, but in most Greek contexts, **\~0.3 persons/ha/year** is realistic \[[4](https://www.researchgate.net/publication/228584613_Forest_land_use_changes_in_Greece_An_ordinal_regression_analysis_at_the_regional_level), [10](https://www.sciencedirect.com/science/article/abs/pii/S0048969721024797)].
;
;These values help in comparative land suitability or carrying capacity models.
;
;## 🌐 Sources
;
;1. [gallifreypermaculture.com.au - Olives for integrated livestock-orchards](http://gallifreypermaculture.com.au/2016/11/olives-for-integrated-livestock-orchards/)
;2. [researchgate.net - Land cover changes in Euboea island using Sentinel-2 ...](https://www.researchgate.net/publication/374618663_Land_cover_changes_in_Euboea_island_using_Sentinel-2_datasets)
;3. [researchgate.net - farm size and feed self-sufficiency matter](https://www.researchgate.net/publication/367327307_Productivity_gains_evolution_of_productive_performances_and_profitability_of_organic_ruminant_farms_farm_size_and_feed_self-sufficiency_matter)
;4. [researchgate.net - Forest land use changes in Greece: An ordinal regression ...](https://www.researchgate.net/publication/228584613_Forest_land_use_changes_in_Greece_An_ordinal_regression_analysis_at_the_regional_level)
;5. [mdpi.com - Profitability, Productivity, and Technical Efficiency of Cretan ...](https://www.mdpi.com/2077-0472/13/12/2194)
;6. [mdpi.com - Projected Impacts of Climate and Land Use Change on ...](https://www.mdpi.com/2225-1154/13/5/100)
;7. [researchgate.net - Diet selection and performance of sheep and goats grazing ...](https://www.researchgate.net/publication/234092558_Diet_selection_and_performance_of_sheep_and_goats_grazing_on_different_heathland_vegetation_types)
;8. [lidsen.com - Energy Self-Sufficiency in Rural Areas; Case Study](https://www.lidsen.com/journals/aeer/aeer-05-04-025)
; 9. [sciencedirect.com - Conditioned aversion to olive tree leaves (Olea europaea L ...](https://www.sciencedirect.com/science/article/abs/pii/S0168159110002601)
; 10. [sciencedirect.com - Land cover and vegetation carbon stock changes in Greece](https://www.sciencedirect.com/science/article/abs/pii/S0048969721024797)

end

to-report get-carrying-capacity-demand-at-age [ age ]

  let carrying-capacity-demand 1

  if (age < 15) [ set carrying-capacity-demand (age / 15) ^ 1.2 ]

  if (age > 45) [ set carrying-capacity-demand (45 / age) ^ 2 ]

  report carrying-capacity-demand

end

to-report get-labour-at-age [ age ]

  let labour 1

  if (age < 15) [ set labour (age / 15) ^ 10 ]

  if (age > 45) [ set labour (45 / age) ^ 10 ]

  report labour

end

to-report get-labour-requirement-per-ha [ landUse ]

  report item (position landUse land-use-list) labour-requirement-list

  ;;; NOTE on estimation of labour required per hectare per year of each land use type (ChatGPT):
; # With pre-industrial technology, what could be reasonable estimates for the labour (adult person per hectare per year) required in each land use category?
;## 🗒️ Answer
;
;With pre-industrial (manual or animal-powered) farming, estimated **labour input per hectare per year** (in adult person-equivalents) varies by land use due to differing intensities:
;
;1. **Grain cultivation: \~1.0–1.2 persons/ha/year**
;
;   * Cereal farming required around 120–150 days of labor per hectare annually, or roughly the full-time effort of one adult for a year. Tasks include plowing, sowing, weeding, harvesting, and threshing \[[4](https://www.jstor.org/stable/2598364), [6](https://baripedia.org/wiki/Agrarian_Structures_and_Rural_Society:_Analysis_of_the_Preindustrial_European_Peasantry)].
;
;2. **Other agriculture (e.g. olives, herding): \~0.3–0.5 persons/ha/year**
;
;   * Olive groves and pastoralism are less labor-intensive. Olives require seasonal pruning and harvest; goats/sheep grazing involves periodic herding and maintenance \[[6](https://baripedia.org/wiki/Agrarian_Structures_and_Rural_Society:_Analysis_of_the_Preindustrial_European_Peasantry)].
;
;3. **Forestry: \~0.1–0.2 persons/ha/year**
;
;   * Forestry involved minimal active management per hectare. Activities like resin tapping, wood cutting, or forest grazing were typically seasonal and low intensity \[[6](https://baripedia.org/wiki/Agrarian_Structures_and_Rural_Society:_Analysis_of_the_Preindustrial_European_Peasantry)].
;
;These values reflect annualized, average labor under subsistence or early agrarian economies.
;
;## 🌐 Sources
;
;1. [pre-sustainability.com - Obscure impacts demystified: Land use](https://pre-sustainability.com/articles/obscure-impacts-demystified-land-use/)
;2. [sciencedirect.com - Labour Market Effects of Large-Scale Agricultural Investment](https://www.sciencedirect.com/science/article/pii/S0305750X17301754)
;3. [link.springer.com - Balancing the trade-offs between land productivity, labor ...](https://link.springer.com/article/10.1007/s13280-023-01887-4)
;4. [jstor.org - Divisions of Labour: Agricultural Productivity and ...](https://www.jstor.org/stable/2598364)
;5. [mdpi.com - A Danish Agricultural Case Study Comparing Sustainability ...](https://www.mdpi.com/2071-1050/13/22/12926)
;6. [baripedia.org - Agrarian Structures and Rural Society: Analysis of the ...](https://baripedia.org/wiki/Agrarian_Structures_and_Rural_Society:_Analysis_of_the_Preindustrial_European_Peasantry)

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; COUNTERS AND MEASURES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to reset-counters

  set womenBirths 0
  set menBirths 0
  set womenDeaths 0
  set menDeaths 0

end

to update-counters

  set totalHouseholds count households
  let oldTotalIndividual totalIndividuals
  set totalIndividuals 0
  set totalWomen 0
  set totalMen 0
  set femaleRatio -1
  set womenAgeStructure (list)
  set menAgeStructure (list)
  set womenFirstAgeGroup 0
  set menFirstAgeGroup 0

  ask households
  [
    foreach hh_membersIndexes
    [
      i ->
      ifelse (item i hh_membersSex)
      [
        set totalWomen totalWomen + 1
        set womenAgeStructure lput item i hh_membersAge womenAgeStructure
        if (item i hh_membersAge < 5)
        [ set womenFirstAgeGroup womenFirstAgeGroup + 1 ]
      ]
      [
        set totalMen totalMen + 1
        set menAgeStructure lput item i hh_membersAge menAgeStructure
        if (item i hh_membersAge < 5)
        [ set menFirstAgeGroup menFirstAgeGroup + 1 ]
      ]
      set totalIndividuals totalIndividuals + 1
    ]
  ]

  carefully [ set totalPopulationGrowth 100 * (totalIndividuals - oldTotalIndividual) / oldTotalIndividual ] [ ]

  carefully [ set femaleRatio totalWomen / totalIndividuals ] [ set femaleRatio "" ]

  carefully [ set totalOrphans length orphanList ] [ set totalOrphans 0 ]

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DISPLAY ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to update-display

  paint-patches

  display-rivers

  display-sites

end

to paint-patches

  if (display-mode = "elevation")
  [
    ask patches
    [
      set pcolor get-elevation-color elevation
    ]
  ]
;  if (display-mode = "distance-to-rivers")
;  [
;    ask patches
;    [
;      set pcolor get-gradient-color distance-to-rivers 0 maxDistRivers
;    ]
;  ]
  if (display-mode = "productivity")
  [
    ask patches
    [
      set pcolor get-gradient-color productivity 0 max-productivity
    ]
  ]
  if (display-mode = "carrying-capacity-contribution-per-ha")
  [
    ask patches
    [
      set pcolor get-gradient-color carrying-capacity-contribution-per-ha min-carrying-capacity-contribution-per-ha max-carrying-capacity-contribution-per-ha
    ]
  ]

  if (show-land-use)
  [
    ;;; "grain crops" "other crops" "forest"
    let color-index [ white yellow brown ]

    ask patches with [land-use != item 3 land-use-list]
    [
      set pcolor item (position land-use land-use-list) color-index
      ;if (land-use = item 0 land-use-list) [ set pcolor item 0 color-index ]
      ;if (land-use = item 1 land-use-list) [ set pcolor item 1 color-index ]
      ;if (land-use = item 2 land-use-list) [ set pcolor item 2 color-index ]
      ;;;if (land-use = "Water bodies") [ set pcolor item 3 color-index ]
    ]

    ;display-shape
  ]

  if (show-territories)
  [
    ask sites [ site_highlight-territory ]
  ]

end

to-report get-elevation-color [ elevationValue ]

  if (elevationValue = -9999) [ report rgb 0 0 100 ]

  let seaLevel 0
  let elevationGradient 0

  ifelse (elevationValue < seaLevel)
  [
    let normSubElevation (-1) * (seaLevel - elevationValue)
    let normSubminElevation (-1) * (seaLevel - min-elevation) + 1E-6
    set elevationGradient 100 + (100 * (1 - normSubElevation / normSubminElevation))
    report rgb 0 0 elevationGradient
  ]
  [
    let normSupElevation elevationValue - seaLevel
    let normSupmax-elevation max-elevation - seaLevel + 1E-6
    set elevationGradient 100 + (155 * (normSupElevation / normSupmax-elevation))
    report rgb (elevationGradient - 100) elevationGradient 0
  ]

end

to-report get-gradient-color [ aValue minValue maxValue ]

  if (aValue = -9999 or not (aValue < 0 or aValue >= 0) ) [ report rgb 0 0 100 ]

  let valueGradient 50 + 200 * (aValue - minValue) / (maxValue - minValue)

  report rgb valueGradient 10 10

end

to display-shape

  foreach gis:feature-list-of agricultural-land-data
  [
    feature ->
    gis:set-drawing-color red
    gis:draw feature 1
  ]

end

to display-rivers

  gis:set-drawing-color blue
  gis:draw rivers-data 1

end

to display-sites

  display-sites-population

  display-sites-land-use

end

to display-sites-population

  ifelse (show-sites)
  [
    let max-population-in-a-site max [site_count-population] of sites

    ask sites
    [
      set hidden? false
      set size 1 + 6 * site_count-population / (max-population-in-a-site + 1E-6)
    ]
  ]
  [
    ask sites [ set hidden? true ]
  ]

end

to display-sites-land-use

  if (show-territories)
  [
    ask sites [ site_highlight-territory ]
  ]

  if (show-sites-land-use)
  [
    ask sites [ site_highlight-territory-in-use ]
  ]

end

to plot-table [ values ]

  let j 0
  foreach values
  [
    i ->
    plotxy j i
    set j j + 1
  ]
  plot-pen-up

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DEMOGRAPHY TABLES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to build-demography-tables

  ;;; load demographic data tables into lists

  ;=======FERTILITY========================================================

  build-fertility-tables

  ;=======NUPTIALITY========================================================

  build-nuptiality-tables

  ;=======MORTALITY========================================================

  build-mortality-tables

end


to build-fertility-tables

  set fertility-table load-peristeri-kostaki-model-table c1-fert mu-fert sigma1-fert sigma2-fert

end

to build-nuptiality-tables

  set nuptiality-table-women load-peristeri-kostaki-model-table c1-women mu-women sigma1-women sigma2-women

  set nuptiality-table-men load-peristeri-kostaki-model-table c1-men mu-men sigma1-men sigma2-men

end

to-report load-peristeri-kostaki-model-table [ c1 mu sigma1 sigma2 ]

  ;;; The following correspond to the first parametric model in:

  ;;; Peristeva and Kostaki, 2009, p. 147
  ;;; "Modeling fertility in modern populations"
  ;;; Demographic Research 16: 141-194
  ;;; Available from: https://dx.doi.org/10.4054/DemRes.2007.16.6

  ;;; Peristeva and Kostaki (2015), p. 133
  ;;; "A parametric model for estimating nuptiality patterns in modern populations"
  ;;; Canadian studies in population 42(2):130-148. DOI: 10.25336/P6TK56
  ;;; Available from: https://www.researchgate.net/publication/285457704_A_parametric_model_for_estimating_nuptiality_patterns_in_modern_populations [accessed Nov 27 2018].
  ;;; use "demoTables/compareNuptialityModel.R" to test shapes

  let marriageProbs (list)

  foreach n-values 151 [ i -> i ]
  [
    i ->
    let sigma sigma1
    if (i > mu) [ set sigma sigma2 ]

    set marriageProbs lput (
      c1 * exp (-1 * (((i - mu) / sigma) ^ 2))
    ) marriageProbs
  ]

  report marriageProbs

end

to build-mortality-tables

  set mortality-table-women load-coale-demeny-table true

  set mortality-table-men load-coale-demeny-table false

end

to-report load-coale-demeny-table [ isFemale ]

  ;;; Coale-Demeny Life Tables Model
  ;;; tables generated with 'cdmlt' functions in 'demoR' package
  ;;; demoR package version 0.6.0 (2018-09-13)
  ;;; by James Holland Jones and collaborators
  ;;; Their source:
  ;;; Coale, A., P. Demeny, and B. Vaughn. 1983.
  ;;; Regional model life tables and stable populations.
  ;;; 2nd ed. New York: Academic Press.

  ;;; Tables were generated using the R script 'demoTables/importCoaleDemenyLifeTables.R'
  ;;; included in the demoTables folder.

  ;;; this function assumes there is a text file (demoTables/cdmlt<coale-demeny-region><sex>.txt)
  ;;; containing a matrix with prob. of death for life-expentancy-level (rows) by age cohort (columns).
  ;;; values of the first row and columns should be skipped

  let sex "F"
  if (not isFemale) [ set sex "M" ]

  let nqx (list)

  ; read file corresponding to coale-demeny-region and sex
  let FilePath "demoTables//"
  let filename (word FilePath "cdmlt" (first coale-demeny-region) sex ".txt")

  file-open filename

  ; skip first line (header)
  let temp file-read-line

  ; read lines and get the one corresponding to coale-demeny-life-expectancy-level
  let i 1
  while [not file-at-end?]
  [
    ; read line
    set temp read-from-string (word "[" file-read-line "]")
    ; select value corresponding to coale-demeny-life-expectancy-level (because the first item is 0, the values skips the first column or row names)
    set nqx lput (item cdmlt-level temp) nqx

;    if (read-from-string item 0 temp = cdmlt-level)
;    [
;      set nqx remove-item 0 temp
;    ]
    set i i + 1
  ]

  file-close

  report nqx

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; GIS DATA LOAD AND PREPARATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to load-gis

  let period-list [
    "3000BC"
    "1500BC"
    "500BC"
    "500CE"
  ]

  let dem-period-list [
    "-30"
    "-15"
    "-5"
    "5"
    "20"
  ]

  let dem-period item (position period period-list) dem-period-list

  set elevation-data gis:load-dataset (word "gis/elevation/Eretria_CHELSA_TraCE21k_dem_" dem-period "_V1.asc")


  ;;; Euboea_Corine_historical = these polygons are the agricultural areas for the entire island,
  ;;; as they possibly were in the past, according to the expert knowledge who used the modern corine land use data and corrected it.
  ; set agricultural-land-data gis:load-dataset "gis/Euboea_Corine_historical_proj/Euboea_Corine_historical_proj.shp";"gis/Euboea_Corine_historical_reprojected.shp"

  set rivers-data gis:load-dataset "gis/rivers/Eretria_rivers.shp"
  ;set distRivers-data gis:load-dataset "gis/rivers/Eretria_riverDistance.asc"

  set productivity-data gis:load-dataset (word "gis/productivity/productivity_" period ".asc")
  ;;; NOTE Raster calculation on QGIS: PRODUCTIVITY = 0.24 *  ( 1 - DEM )  + 0.4 *  ( 1 - SLOPE )  + 0.36 *  ( 1 - DIST_RIVERS )

  set agricultural-land-data gis:load-dataset "gis/land use/agricultural_land.shp"

  set forest-land-data gis:load-dataset "gis/land use/forest_land.shp"

  set grain-land-data gis:load-dataset "gis/land use/grain_land.shp"

  set land-use-list [ "grain crops" "other crops" "forest" "none" ]

  set sites-data gis:load-dataset "gis/maindemeseretriashp/Main_Cities_Eretria.shp"

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EXPORT DATA ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to export-households

  let FilePath "output//"

  file-open (word FilePath behaviorspace-experiment-name behaviorspace-run-number "_households.csv")

  file-print "household,lineage,householdAge,maxCoupleCount,membersAge,membersSex,membersMarriage"

  foreach sort households
  [
    hh ->
    ask hh
    [
      file-type who file-type ", "
      file-type (word "'" hh_lineage "'") file-type ", "
      file-type hh_age file-type ", "
      file-type hh_maxCoupleCount file-type ", "
      file-type (word "'" hh_membersAge "'") file-type ", "
      file-type (word "'" hh_membersSex "'") file-type ", "
      file-print (word "'" hh_membersMarriage "'")
    ]
  ]

  file-close

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;; numeric generic functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report clamp01 [ value ]
  report min (list 1 (clampMin0 value))
end

to-report clampMin0 [ value ]
  report (max (list 0 value))
end

to-report clampMinMax [ value minValue maxValue ]
  report min (list maxValue (max (list minValue value)))
end
@#$#@#$#@
GRAPHICS-WINDOW
1160
10
1723
494
-1
-1
5.0
1
10
1
1
1
0
1
1
1
0
110
0
94
0
0
1
ticks
30.0

BUTTON
15
10
78
43
NIL
setup
NIL
1
T
OBSERVER
NIL
1
NIL
NIL
1

BUTTON
93
11
148
44
NIL
go
NIL
1
T
OBSERVER
NIL
2
NIL
NIL
1

INPUTBOX
6
55
80
115
SEED
3.0
1
0
Number

CHOOSER
54
133
192
178
type-of-experiment
type-of-experiment
"user-defined" "random"
0

INPUTBOX
273
99
441
159
household-initial-age-distribution
0 30
1
0
String

INPUTBOX
281
219
437
279
max-couple-count-distribution
1 6
1
0
String

PLOT
460
311
897
431
Households
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"households" 1.0 0 -16777216 true "plot totalHouseholds" "plot totalHouseholds"
"couples" 1.0 0 -7500403 true "" "plot sum [hh_count-couples] of households"

PLOT
459
590
863
710
Age structure (population)
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"set-histogram-num-bars 20\nset-plot-x-range -1 max (sentence womenAgeStructure menAgeStructure)" "set-plot-y-range 0 10\n;set-histogram-num-bars 20\ncarefully [ set-plot-x-range -1 max (sentence womenAgeStructure menAgeStructure) ] [ ]"
PENS
"women" 1.0 1 -2674135 true "" "carefully [ histogram womenAgeStructure ] [ ]"
"men" 1.0 1 -13345367 true "" "carefully [ histogram menAgeStructure ] [ ]"

MONITOR
784
644
857
689
NIL
femaleRatio
4
1
11

PLOT
975
568
1156
782
Household structure
members count
mean members age
0.0
10.0
0.0
10.0
true
false
"" "clear-plot"
PENS
"" 1.0 2 -16777216 true "" "ask households [ plotxy (length hh_membersAge) (mean hh_membersAge) ]"

PLOT
459
10
877
160
Population
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"total" 1.0 0 -16777216 true "" "plot totalIndividuals"
"women" 1.0 0 -2674135 true "" "plot totalWomen"
"men" 1.0 0 -13345367 true "" "plot totalMen"
"mean  K" 1.0 0 -7500403 true "" "plot sum [carrying-capacity] of sites"

PLOT
460
431
896
557
maxCoupleCount and actual count
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"set-plot-y-range -1 (1 + item 1 maxCoupleCountDistribution)" ""
PENS
"max. mean" 1.0 0 -8053223 true "" "carefully [ plot mean [hh_maxCoupleCount] of households] [ ]"
"max. max" 1.0 0 -8630108 true "" "carefully [ plot max [hh_maxCoupleCount] of households ] [ ]"
"max. min" 1.0 0 -5825686 true "" "carefully [ plot min [hh_maxCoupleCount] of households ] [ ]"
"count mean" 1.0 0 -15582384 true "" "carefully [ plot mean [hh_count-couples] of households ] [ ]"
"count max" 1.0 0 -14454117 true "" "carefully [ plot max [hh_count-couples] of households ] [ ]"
"count min" 1.0 0 -12345184 true "" "carefully [ plot min [hh_count-couples] of households ] [ ]"

MONITOR
918
166
1019
211
women death rate
womenDeaths / totalWomen
4
1
11

MONITOR
1045
164
1145
209
men death rate
menDeaths / totalMen
4
1
11

PLOT
460
160
911
310
Births and deaths
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"women deaths" 1.0 0 -11053225 true "" "plot womenDeaths"
"men deaths" 1.0 0 -4539718 true "" "plot menDeaths"
"births" 1.0 0 -14439633 true "" "plot womenBirths + menBirths"
"deaths" 1.0 0 -16777216 true "" "plot womenDeaths + menDeaths"
"women births" 1.0 0 -13210332 true "" "plot womenBirths"
"men births" 1.0 0 -11881837 true "" "plot menBirths"

MONITOR
918
120
1011
165
women birth rate
womenBirths / totalWomen
4
1
11

MONITOR
1045
119
1138
164
men birth rate
menBirths / totalMen
4
1
11

BUTTON
159
11
214
44
NIL
go
T
1
T
OBSERVER
NIL
3
NIL
NIL
1

SLIDER
237
384
440
417
cdmlt-level
cdmlt-level
1
25
16.0
1
1
levels from 1 to 25
HORIZONTAL

CHOOSER
236
417
377
462
coale-demeny-region
coale-demeny-region
"west" "east" "south" "north"
2

PLOT
10
349
216
489
mortality (prob. dying)
age
NIL
0.0
10.0
0.0
10.0
false
false
"" "clear-plot\nset-plot-x-range 0 100\nset-plot-y-range -0.001 (precision (max (list max mortality-table-women max mortality-table-men) + 0.001) 4)"
PENS
"women" 1.0 0 -5298144 true "\n" "plot-table mortality-table-women"
"men" 1.0 0 -14070903 true "" "plot-table mortality-table-men"
"women (mod)" 1.0 0 -2674135 true "" "let meanDensityEffect mean [site_get-density-effect] of sites\nplot-table map [i -> i + meanDensityEffect] mortality-table-women"
"men (mod)" 1.0 0 -10649926 true "" "let meanDensityEffect mean [site_get-density-effect] of sites\nplot-table map [i -> i + meanDensityEffect] mortality-table-men"

PLOT
11
633
217
789
nuptiality (prob. marrying)
age
NIL
0.0
10.0
0.0
10.0
false
false
"" "clear-plot\nset-plot-x-range 0 55\nset-plot-y-range -0.001 (precision (max (list max nuptiality-table-women max nuptiality-table-men) + 0.001) 4)"
PENS
"women" 1.0 0 -5298144 true "" "plot-table nuptiality-table-women"
"men" 1.0 0 -14070903 true "" "plot-table nuptiality-table-men"

PLOT
10
489
217
632
fertility (prob. of giving birth)
age
NIL
0.0
10.0
0.0
10.0
false
false
"" "clear-plot\nset-plot-x-range 0 55\nset-plot-y-range -0.001 (precision (max fertility-table + 0.001) 4)"
PENS
"women" 1.0 0 -5298144 true "" "plot-table fertility-table"

PLOT
897
431
1097
551
couple count
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"set-plot-y-range -0.001 (item 1 maxCoupleCountDistribution + 0.01)" ""
PENS
"default" 1.0 1 -16777216 true "" "histogram [hh_count-couples] of households"

CHOOSER
222
637
379
682
residence-rule
residence-rule
"patrilocal-patrilineal" "matrilocal-matrilineal" "random"
2

MONITOR
868
620
968
665
% 0-4 (women)
100 * womenFirstAgeGroup / totalWomen
2
1
11

MONITOR
868
664
968
709
% 0-4 (men)
100 * menFirstAgeGroup / totalMen
2
1
11

MONITOR
919
22
1129
79
total population growth (%)
totalPopulationGrowth
2
1
14

SLIDER
224
722
407
755
c1-women
c1-women
0
1
0.95
0.001
1
(default: 0.9)
HORIZONTAL

SLIDER
589
719
775
752
sigma1-women
sigma1-women
0
2 * 5
2.0
0.001
1
(default: 5)
HORIZONTAL

SLIDER
407
721
588
754
mu-women
mu-women
0
40
20.0
0.001
1
(default: 15)
HORIZONTAL

SLIDER
223
755
407
788
c1-men
c1-men
0
1
0.95
0.001
1
(default: 0.85)
HORIZONTAL

SLIDER
407
754
589
787
mu-men
mu-men
0
2 * 20
20.0
0.001
1
(default: 20)
HORIZONTAL

SLIDER
589
754
790
787
sigma1-men
sigma1-men
0
2 * 5
2.0
0.001
1
(default: 2)
HORIZONTAL

SLIDER
220
490
430
523
c1-fert
c1-fert
0
1
0.95
0.001
1
(default: 0.9)
HORIZONTAL

SLIDER
220
558
455
591
sigma1-fert
sigma1-fert
0
2 * 5
3.0
0.001
1
(default: 5)
HORIZONTAL

SLIDER
222
593
456
626
sigma2-fert
sigma2-fert
0
6 * 5
15.0
0.001
1
(default: 10)
HORIZONTAL

SLIDER
221
524
429
557
mu-fert
mu-fert
0
40
17.0
0.001
1
(default: 15)
HORIZONTAL

BUTTON
274
339
383
372
refresh tables
build-demography-tables\nupdate-plots
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
918
215
1044
260
women immigration rate
womenIn / totalWomen
4
1
11

MONITOR
1046
216
1157
261
men immigration rate
menIn / totalMen
4
1
11

MONITOR
919
264
1041
309
women emigration rate
womenOut / totalWomen
4
1
11

MONITOR
1047
264
1155
309
men emigration rate
menOut / totalMen
4
1
11

MONITOR
286
172
428
209
NIL
houseHoldInitialAgeDistribution
17
1
9

MONITOR
295
291
423
328
NIL
maxCoupleCountDistribution
0
1
9

TEXTBOX
284
278
440
301
<minimum><SPACE><maximum>
9
0.0
1

TEXTBOX
278
158
434
180
<minimum><SPACE><maximum>
9
0.0
1

MONITOR
1001
326
1084
371
orphans count
totalOrphans
0
1
11

SLIDER
776
718
965
751
sigma2-women
sigma2-women
0
2 * 5
10.0
0.001
1
(default: 5)
HORIZONTAL

SLIDER
777
753
965
786
sigma2-men
sigma2-men
0
2 * 5
10.0
0.001
1
(default: 10)
HORIZONTAL

SLIDER
223
685
444
718
acceptable-kinship-degree-for-couples
acceptable-kinship-degree-for-couples
0
10
9.0
1
1
NIL
HORIZONTAL

INPUTBOX
81
55
161
115
max-iterations
2000.0
1
0
Number

BUTTON
55
196
190
229
NIL
export-households
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
162
55
249
115
max-population
100000.0
1
0
Number

BUTTON
46
235
198
268
parameters to default
parameters-to-default
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
1179
565
1568
598
max-carrying-capacity-contribution-per-ha-grain
max-carrying-capacity-contribution-per-ha-grain
2
5
4.0
0.01
1
ind/ha/year
HORIZONTAL

SLIDER
4
291
269
324
density-effect-scaling-factor
density-effect-scaling-factor
0
0.5
0.05
0.01
1
(default: 0.05)
HORIZONTAL

SLIDER
1179
598
1571
631
max-carrying-capacity-contribution-per-ha-other
max-carrying-capacity-contribution-per-ha-other
0
5
2.5
0.01
1
ind/ha/year
HORIZONTAL

SLIDER
1179
630
1574
663
max-carrying-capacity-contribution-per-ha-forest
max-carrying-capacity-contribution-per-ha-forest
0
5
0.3
0.01
1
ind/ha/year
HORIZONTAL

SLIDER
1536
691
1740
724
max-catchment-distance
max-catchment-distance
0
10
5.0
1
1
km
HORIZONTAL

CHOOSER
1179
506
1317
551
period
period
"3000BC" "1500BC" "500BC" "500CE"
0

CHOOSER
1347
509
1550
554
display-mode
display-mode
"elevation" "productivity" "carrying-capacity-contribution"
0

SWITCH
1581
522
1702
555
show-land-use
show-land-use
0
1
-1000

BUTTON
1597
601
1691
634
refresh display
update-display
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
1580
556
1702
589
show-territories
show-territories
1
1
-1000

SLIDER
1179
675
1476
708
labour-requirement-per-ha-grain
labour-requirement-per-ha-grain
0
2
1.2
0.01
1
ind/ha/year
HORIZONTAL

SLIDER
1181
708
1481
741
labour-requirement-per-ha-other
labour-requirement-per-ha-other
0
2
0.5
0.01
1
ind/ha/year
HORIZONTAL

SLIDER
1181
740
1484
773
labour-requirement-per-ha-forest
labour-requirement-per-ha-forest
0
2
0.2
0.01
1
ind/ha/year
HORIZONTAL

SWITCH
1703
522
1817
555
show-sites
show-sites
1
1
-1000

SWITCH
1702
555
1846
588
show-sites-land-use
show-sites-land-use
1
1
-1000

INPUTBOX
267
10
441
70
initial-num-households-per-site
25.0
1
0
Number

MONITOR
284
69
433
106
NIL
initialNumHouseholdsPerSite
0
1
9

SLIDER
1536
724
1741
757
max-coastal-bonus
max-coastal-bonus
0
100
20.0
1
1
%
HORIZONTAL

@#$#@#$#@
## Development notes

TASK:
Add parametric tabu restrictions to the formation of couples. Now, it is possible to have any given single individual marrying any other single, even siblings and, less likely, mother-son/father-daughter... 
DONE

IMPROVEMENTS:
- apply-nuptiality: now this procedure follows a more elegant and clear process

ISSUES:
- Households now track their lineage and match making is controlled by the similarity of lineage. However, this mechanisms reflects a purely lineal track of descent, meaning that relatively close blood relatives are not tracked if they do not share the lineage; e.g., a man under a matrilocal-matrilineal rule will loose track of all 'former' blood relative once they move away from their original household. Unfortunately, this solution still do not control for extreme rare cases such as marriages between siblings and other close relatives, if considering widowing. The only definitive solution is too increase complexity of the information at individual level.
- Initialisation: the result given by the procedure 'get-kinship-degree' assumes all initial households descend from a single common parent household (returns 2). So, if the acceptable kinship level is more than 2, the formation of couples during the first generation will be restricted to those using external individuals ('create-couple-external').

## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.4.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="exp-trajectories" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>SEED</metric>
    <metric>maturityAge</metric>
    <metric>initialNumHouseholds</metric>
    <metric>householdInitialAgeDistribution</metric>
    <metric>maxCoupleCountDistribution</metric>
    <metric>cdmlt-level</metric>
    <metric>coale-demeny-region</metric>
    <metric>c1-fert</metric>
    <metric>mu-fert</metric>
    <metric>sigma1-fert</metric>
    <metric>sigma2-fert</metric>
    <metric>residence-rule</metric>
    <metric>acceptable-kinship-degree-for-couples</metric>
    <metric>c1-women</metric>
    <metric>c1-men</metric>
    <metric>mu-women</metric>
    <metric>mu-men</metric>
    <metric>sigma1-women</metric>
    <metric>sigma1-men</metric>
    <metric>sigma2-women</metric>
    <metric>sigma2-men</metric>
    <metric>totalHouseholds</metric>
    <metric>totalIndividuals</metric>
    <metric>totalPopulationGrowth</metric>
    <metric>totalWomen</metric>
    <metric>totalMen</metric>
    <metric>femaleRatio</metric>
    <metric>womenAgeStructure</metric>
    <metric>menAgeStructure</metric>
    <metric>womenFirstAgeGroup</metric>
    <metric>menFirstAgeGroup</metric>
    <metric>womenBirths</metric>
    <metric>menBirths</metric>
    <metric>womenDeaths</metric>
    <metric>menDeaths</metric>
    <metric>womenIn</metric>
    <metric>menIn</metric>
    <metric>womenOut</metric>
    <metric>menOut</metric>
    <metric>totalOrphans</metric>
    <steppedValueSet variable="SEED" first="0" step="1" last="99"/>
  </experiment>
  <experiment name="exp-endstates" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>SEED</metric>
    <metric>maturityAge</metric>
    <metric>initialNumHouseholds</metric>
    <metric>householdInitialAgeDistribution</metric>
    <metric>maxCoupleCountDistribution</metric>
    <metric>cdmlt-level</metric>
    <metric>coale-demeny-region</metric>
    <metric>c1-fert</metric>
    <metric>mu-fert</metric>
    <metric>sigma1-fert</metric>
    <metric>sigma2-fert</metric>
    <metric>residence-rule</metric>
    <metric>acceptable-kinship-degree-for-couples</metric>
    <metric>c1-women</metric>
    <metric>c1-men</metric>
    <metric>mu-women</metric>
    <metric>mu-men</metric>
    <metric>sigma1-women</metric>
    <metric>sigma1-men</metric>
    <metric>sigma2-women</metric>
    <metric>sigma2-men</metric>
    <metric>totalHouseholds</metric>
    <metric>totalIndividuals</metric>
    <metric>totalPopulationGrowth</metric>
    <metric>totalWomen</metric>
    <metric>totalMen</metric>
    <metric>femaleRatio</metric>
    <metric>womenAgeStructure</metric>
    <metric>menAgeStructure</metric>
    <metric>womenFirstAgeGroup</metric>
    <metric>menFirstAgeGroup</metric>
    <metric>womenBirths</metric>
    <metric>menBirths</metric>
    <metric>womenDeaths</metric>
    <metric>menDeaths</metric>
    <metric>womenIn</metric>
    <metric>menIn</metric>
    <metric>womenOut</metric>
    <metric>menOut</metric>
    <metric>totalOrphans</metric>
    <steppedValueSet variable="SEED" first="0" step="1" last="4999"/>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
