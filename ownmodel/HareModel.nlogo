;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GLOBALS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


extensions
[
  profiler
  Rnd
  Table
]

globals
[
  cropProbability          ;; list            list of cultivation probabilities for each crop
  cropToSuit               ;; table           table with foraging and breeding values for each crop
  crop-list                ;; [number]        list of all crops within the landscape
  fieldID-list             ;; [number]        list of all field IDs within the landscape
  fieldList                ;; list            list of fieldIDs and number of patches for each field
  fieldTable               ;; table           table with fieldIDs and number of patches for each field
  homeRangeRadius          ;; [cells]         radius of the home range
  initialPopulation        ; to check population size: show count turtles with [status = "female" or status = "male"]  / 16
  maximumOverlap           ;; [number]        maximum number of home ranges overlapping
  maximumOwners            ;; [number]        maximum number of owners assigned to a search cell
  richness                 ;; [number]        richness of crop types within the landscape
  suitabilityReduction     ;; [number]        reduction of the habitat suitability value when home ranges overlap
  thresholdSuitability     ;; [#]              minimum threshold for habitat suitability<
  totalcrops               ;; [#]             number of crops in the landscape
  totalfields              ;; [#]             number of fields in the landscape
]

turtles-own
[
  age                      ;; [a]             age of the individuals
  homeRange                ;; [cells]         grid cells which belong to the homeRange
  homeRangeNumber          ;; [number]        number of cells which belong to the homeRange VV is the same for all
  longevity                ;; [a]             maximum age
  maturity                 ;; [a]             sexual maturity
  mortalityAdult           ;; [%]             mortality rate of adults
  mortalityJuvenile        ;; [%]             mortality rate of juveniles
  offspring                ;; [number, 12-15] females get 12-15 offspring each year
  suithomeRange            ;; [number, 0-1]   mean habitat suitability of all grid cells within a homeRange
  status                   ;; [string]        hare specification: juvenile, female, male
]

patches-own
[
  crop                     ;; [string]        crop type grown on the field
  breeding                 ;; [number, 0-1]   suitability as breeding habitat
  fieldID                  ;; [#]             field ID number
  foraging                 ;; [number, 0-1]   suitability as forage habitat
  numberOwners             ;; [#]             number of hares to whose homeRange the cell belongs to
  owner                    ;; [turtle]        owner of the patch
  suitability              ;; [number, 0-1]   habitat suitability of the cell
]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SETUP + GO ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; SETUP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to setup

     clear-all
     set initialPopulation 80
     init_landscape                 ;; imports the landscape from a text file
     cultivate                      ;; crops are cultivated on the fields
     init_calculate-suitability     ;; calculates habitat suitability for each cell
     init_hares                     ;; ceates the initial population
     init_search-homeRange          ;; hares search for a suitable, occupyable homeRange
     init_calculate-suithomeRange   ;; calculates habitat suitability of the homeRange
     update-view

     reset-ticks
     reset-timer

end


;; GO ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to go
     if (ticks = 20) [
       print  "Number of turtles per km2: "
       print (count turtles with [status = "female" or status = "male"] / 16)
       stop
     ]

     population
     cultivate
     calculate-habitat-suitability
     search_homeRange_matures
     reproduce
     survive
     tick

end



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INITIALISATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; LANDSCAPE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to init_landscape

     set fieldID-list []

     file-open "./landscape_6.8ha.txt"
     ;file-open "./landscape_munsterland_27.5ha.txt"

     while [not file-at-end?]
     [
         let next-x file-read
         let next-y file-read
         let next-fieldID file-read
         set fieldID-list fput next-fieldID fieldID-list
         ask patch next-x next-y
         [
             set fieldID next-fieldID
         ]
     ]

     file-close

     ask patches
     [
         set numberOwners 0                                   ;; all cells are unoccupied at the beginning
     ]


;; testing size calculations / only information

    output-print (word "Landscape size: " ((max-pxcor + 1) / 100) " x " ((max-pxcor + 1) / 100) " km")
    output-print (word "Cell size: 10 x 10 m")
    output-print (word " ")

    output-print (word "Home range: " precision ((pi * (homeRangeRadius * 10) ^ 2 ) / 10000 ) 0 " ha")
    output-print (word " ")


end


;; Cultivation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to cultivate

     set fieldTable table:group-agents patches [fieldID]                                                   ;; creates a table with index/key "fieldID" and the agentset of patches as value
     set fieldList table:to-list fieldTable                                                                ;; transforms the table into list of elements, which are lists with elements themselves: the fieldID, and the corresponding set of patches

     let no 0                                                                                              ;; counts entries in the fieldList

     foreach fieldList                                                                                     ;; convert patchsets (all fields with a given ID) to the size of the patchset
     [
         [x] -> set fieldList replace-item no fieldList list (item 0 x) (count item 1 x)                   ;; element number "no" of the fieldList is replaced with a two-item list: [(fieldID) (number of patches with that fieldID)]
         set no no + 1
     ]



     set cropProbability [["wheat" 0.375] ["rape" 0.187] ["maize" 0.150] ["barley" 0.092] ["grassland" 0.053] ["pasture" 0.053]
                         ["beets" 0.045] ["alfalfa" 0.015] ["set-aside" 0.015] ["rye" 0.014]]

     if (Scenario = "Basic")
     [
     set cropProbability [["wheat" 0.207] ["pasture" 0.181] ["maize" 0.178] ["grassland" 0.125] ["barley" 0.113] ["rape" 0.083]
                         ["rye" 0.036] ["beets" 0.028] ["triticale" 0.024] ["set-aside" 0.024]]
     ]

     if (Scenario = "Silphie")
     [
     set cropProbability [["wheat" 0.207] ["pasture" 0.181] ["grassland" 0.125] ["barley" 0.113] ["silphie" 0.100] ["rape" 0.083]
                         ["maize" 0.078] ["rye" 0.036] ["beets" 0.028] ["triticale" 0.024] ["set-aside" 0.024]]
     ]

     if (Scenario = "Miscanthus")
     [
     set cropProbability [["wheat" 0.207] ["pasture" 0.181] ["maize" 0.161] ["grassland" 0.125] ["barley" 0.113]
                         ["miscanthus" 0.100] ["rye" 0.036] ["beets" 0.028] ["triticale" 0.024] ["set-aside" 0.024]]
     ]

     if (Scenario = "Grass-clover ley")
     [
     set cropProbability [["wheat" 0.207] ["pasture" 0.181] ["grassland" 0.125] ["barley" 0.113] ["grass-clover ley" 0.100] ["rape" 0.083]
                         ["maize" 0.078] ["rye" 0.036] ["beets" 0.028] ["triticale" 0.024] ["set-aside" 0.024]]
     ]

     if (Scenario = "Alfalfa")
     [
     set cropProbability [["wheat" 0.207] ["pasture" 0.181] ["maize" 0.161] ["grassland" 0.125] ["barley" 0.113]
                         ["alfalfa" 0.100] ["rye" 0.036] ["beets" 0.028] ["triticale" 0.024] ["set-aside" 0.024]]
     ]

     if (Scenario = "Set-aside")
     [
     set cropProbability [["wheat" 0.207] ["pasture" 0.181] ["maize" 0.178] ["grassland" 0.125] ["barley" 0.113] ["set-aside" 0.100]
                         ["rye" 0.036] ["beets" 0.028] ["triticale" 0.024] ["rape" 0.007]]
     ]

     if (Scenario = "Crop richness")
     [
     set cropProbability [["wheat" 0.140] ["pasture" 0.123] ["maize" 0.120] ["grassland" 0.085] ["barley" 0.077] ["rape" 0.056]
                         ["beets" 0.050] ["alfalfa" 0.050] ["set-aside" 0.050] ["rye" 0.050] ["triticale" 0.050] ["silphie" 0.050]
                         ["miscanthus" 0.050] ["grass-clover ley" 0.050]]
     ]

     ;code above can just be removed I guess - crop probability in some Münsterland areas:
     ; no, we need different scenarios to compare the effects
     if (Scenario = "Muenster")
     [
     set cropProbability [["wheat" 0.35] ["maize" 0.35] ["pasture" 0.1] ["grassland" 0.14] ["barley" 0.02] ["rape" 0.02]
                         ["rye" 0.02]]
     ]

     set cropProbability reverse cropProbability                                                             ;; descending order

     foreach cropProbability                                                                                 ;; go through the list of crop types and their proportions and add, for each croptype, fields
     [
         [x] -> let proportion item 1 x
                let croptype item 0 x

         set proportion proportion * count patches
         let fieldSublist []
         foreach fieldList
         [
             [y] -> if item 1 y <= (proportion * 80 / 100) [ set fieldSublist fput y fieldSublist ]          ;; creates a sublist with fields whose area is (-20 %) smaller than the required crop proportion (proportion - 20 %, to avoid too large sums later)
         ]

         let summe 0                                                                                         ;; sums up field sizes
         let itemNumber 0                                                                                    ;; item number in sublist
         let itemNumber2 0                                                                                   ;; item number in fieldList

         while [ (summe <= (proportion * 85 / 100)) and (length fieldList > 0) ]                             ;; while the sum is (- 15 %) smaller than the required proportion, additional fields are selected from the sublist
         [                                                                                                   ;; proportion - 15 %, to avoid too large sums later
             let selectedField one-of fieldSublist                                                           ;; selects one of fieldSublist
             set itemNumber position selectedField fieldSublist
             set itemNumber2 position selectedField fieldList
             let field item 0 selectedField                                                                  ;; assign fieldID to "field"
             set summe summe + item 1 selectedField                                                          ;; assign field size of this field to "summe"

             ask table:get fieldTable field [ set crop croptype ]                                            ;; assign croptype to this field
             set fieldSublist remove-item itemNumber fieldSublist                                            ;; remove selected field from the sublist
             ifelse length fieldList > 0
             [ set fieldList remove-item itemNumber2 fieldList ]                                             ;; remove selected field from the main list
             [ set summe  99999999 ]                                                                         ;; high value for summe to make the while loop stop
         ]
     ]

     let itemNumber2 0

     while [ length fieldList > 0 ]                                                                          ;; if fields are left, assign the most frequent crop "wheat" to them
     [
         let selectedField one-of fieldList                                                                  ;; assign fieldID to "field", item 0 is the smallest field
         set itemNumber2 position selectedField fieldList
         let field item 0 selectedField
         ask table:get fieldTable field [ set crop "wheat" ]                                                 ;; assign croptype to this field
         set fieldList remove-item itemNumber2 fieldList
     ]

     set fieldID-list remove-duplicates fieldID-list
     set totalfields length fieldID-list                                                                     ;; the number of fields in the landscape is counted

     set crop-list []
     ask patches [ set crop-list fput crop crop-list ]
     set crop-list remove-duplicates crop-list
     set totalcrops length crop-list                                                                         ;; the number of crops in the landscape is counted

end


;; CALCULATE SUITABILITY ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to init_calculate-suitability

     set cropToSuit table:make                                                                               ;; crop is taken as the index of the table, each entry then has a list of two values, for the suitability of foraging and breeding

     table:put CropToSuit "wheat"            [ 0.75 0.75 ]
     table:put CropToSuit "rape"             [ 0.25 0.25 ]
     table:put CropToSuit "maize"            [ 0.50 0.25 ]
     table:put CropToSuit "barley"           [ 0.75 0.75 ]
     table:put CropToSuit "grassland"        [ 0.75 0.75 ]
     table:put CropToSuit "pasture"          [ 0.25 0.25 ]
     table:put CropToSuit "beets"            [ 0.75 0.50 ]
     table:put CropToSuit "alfalfa"          [ 0.75 0.25 ]
     table:put CropToSuit "set-aside"        [ 1.00 1.00 ]
     table:put CropToSuit "rye"              [ 0.50 0.50 ]
     table:put CropToSuit "triticale"        [ 0.50 0.50 ]
     table:put CropToSuit "silphie"          [ 0.50 0.75 ]
     table:put CropToSuit "miscanthus"       [ 0.00 0.25 ]
     table:put CropToSuit "grass-clover ley" [ 0.75 0.50 ]

     if ( totalcrops >= 5) and (totalcrops <= 10)   [ set richness 0.60 ]                                    ;; the crop richness is derived from the number of crops in the landscape
     if ( totalcrops >= 11) and (totalcrops <= 13)  [ set richness 0.80 ]
     if ( totalcrops >= 14)                         [ set richness 1.00 ]

     ask patches
     [
         set foraging (item 0 table:get cropToSuit crop)
         set breeding (item 1 table:get cropToSuit crop)
         set suitability (foraging * breeding * richness)                                                    ;; foraging * breeding * richness
         set suitability suitability ^ (1 / 3)                                                               ;; the geometric mean of foraging, breeding and richness results in the general suitability of the cell
         set suitability precision suitability 2
     ]

end


;; Hares ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to init_hares

     crt initialPopulation

     ask patches [ set owner [] ]

     ask turtles
     [
         ; set shape "rabbit"            ; for the fancy style ;)
         set size 10
         setxy random-pxcor random-pycor

         init_calculate-parameters                                                                           ;; calculates all population parameters that are needed to setup the model

         set age random longevity + maturity                                                                 ;; minimum age 1, maximum age 13
         set status one-of ["female" "male"]                                                                 ;; random distribution of females and males
         if status = "female" [ set color red ]
         if status = "male" [ set color blue ]
         set homeRange patches in-radius homeRangeRadius
         ask homeRange
         [
             set owner fput myself owner
             set numberOwners numberOwners + 1
             if numberOwners > 1 [ set suitability (suitability - suitabilityReduction) ]                    ;; if the cell belongs to two territories, the suitability is reduced by 0.02
         ]
     ]

end

to population
  establish_home_range
  aging
  die_of_longevity
end

to aging
  ask turtles
  [
    set age age + 1
    if status = "juvenile" [
      set status one-of["female" "male"]
      ifelse status = "female" [ set color red ] [ set color blue]
    ]
  ]
end

to establish_home_range
  ask turtles with [status = "juvenile"]
  [
    let counter 0
    let found false
    let numberTrials 3
    let patchesSearched patches in-radius homeRangeRadius with [(numberOwners < maximumOwners) and (suitability >= thresholdSuitability)]
    ifelse not any? patchesSearched [ die ] [
      while [(counter < numberTrials)] [
        move-to one-of patchesSearched
        set homeRange patches in-radius homeRangeRadius
        ifelse not any? homeRange with [numberOwners <= maximumOverlap]
        [ set counter counter + 1 ] ; not sucessfull -> start with the next trial
        [ set homeRange patches in-radius homeRangeRadius
          ask homeRange [ add-to-home ]
          set found true
          set counter numberTrials + 1 ; exit for loop as home range was established sucessfully
        ]
      ]
      if not found [ die ]
    ]
  ]
end

to die_of_longevity
  ask turtles with [age >= longevity] [
    ask homeRange [ remove-from-home ]
    die
  ]
end

;; CALCULATE HARE PARAMETERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to init_calculate-parameters

     set longevity 13
     set maturity 1
     set offspring (12 + random 3)
     set mortalityAdult 0.3
     set mortalityJuvenile 0.5
     set thresholdSuitability 0.5
     set maximumOwners 7
     set maximumOverlap 10
     set suitabilityReduction 0.02
     set homeRangeNumber 2453                               ;; cells which belong to the homeRange
     set homeRangeRadius 28
end


;; SEARCH TERRITORY ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to init_search-homeRange

     ask turtles with [(suithomeRange < thresholdSuitability) or (any? homeRange with [numberOwners > maximumOverlap])]
     [
         ask homeRange
         [
             remove-from-home
         ]
         let found false
         let i 0
         let searchPatches patches in-radius homeRangeRadius with [(numberOwners < maximumOwners) and (suitability >= thresholdSuitability)]
         ifelse any? searchPatches
         [
            while [(not found) and (i < 3)] [

                move-to one-of searchPatches
                set homeRange patches in-radius homeRangeRadius

                ifelse any? homeRange with [numberOwners <= maximumOverlap]
                [
                    ask homeRange
                    [
                        set found true
                        add-to-home
                    ]

                ]
                [
                    set i i + 1
                ]
            ]

            if not found [die]

         ]
         [
             die                                                                                                                  ;; if there are no suitable cells, they die
         ]
     ]

    output-print (word (initialPopulation - count turtles) " hares could not find" )
    output-print (word "a suitable homeRange" )
    output-print (word "at start" )

end

;; CALCULATE HABITAT SUITABILITY OF TERRITORIES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


to init_calculate-suithomeRange

     ask turtles
     [
         set suithomeRange ((sum [suitability] of homeRange) / homeRangeNumber)            ;; the suitability of the homeRange is calculated
         set suithomeRange precision suithomeRange 2
     ]

end

to remove-from-home
    set owner remove myself owner
    set numberOwners numberOwners - 1
    set owner remove nobody owner
    if numberOwners >= 1 [
        set suitability (suitability + suitabilityReduction)
        set suitability precision suitability 2
    ]
end

to add-to-home
    set owner fput myself owner
    set numberOwners numberOwners + 1
    if numberOwners > 1 [ set suitability (suitability - suitabilityReduction) ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Go functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



to calculate-habitat-suitability
    if ( totalcrops >= 5) and (totalcrops <= 10) [ set richness 0.60 ]                                    ;; the crop richness is derived from the number of crops in the landscape
     if ( totalcrops >= 11) and (totalcrops <= 13) [ set richness 0.80 ]
     if ( totalcrops >= 14) [ set richness 1.00 ]

     ask patches
     [
         set foraging (item 0 table:get cropToSuit crop)
         set breeding (item 1 table:get cropToSuit crop)
         set suitability (foraging * breeding * richness)                                                    ;; foraging * breeding * richness
         set suitability suitability ^ (1 / 3)                                                               ;; the geometric mean of foraging, breeding and richness results in the general suitability of the cell
         set suitability precision suitability 2
     ]
end


to calculate-suithomeRange
  init_calculate-suithomeRange
end

to search_homeRange_matures
  let matures turtles with [status != "juvenile" ]

  ;;First try to find a suitable home for every turtle
  ask matures [
    let searchPatches patches in-radius homeRangeRadius with [ ((suitability > thresholdSuitability) and (numberOwners < maximumOwners))]
    if any? searchPatches [
         ask homeRange [
              remove-from-home
         ]
         move-to one-of searchPatches with-max [suitability]                                                             ;; 1st try: hares move to one of the suitable patches
         set homeRange patches in-radius homeRangeRadius
         ask homeRange
         [
             add-to-home
         ]
         set suithomeRange ((sum [suitability] of homeRange) / homeRangeNumber)                                          ;; the suitability of the homeRange is calculated
         set suithomeRange precision suithomeRange 2
    ]

  ]

  ;;Second and third try
  let i 0
  while [i < 2] [
    ask matures with [(suithomeRange < thresholdSuitability) or (any? homeRange with [numberOwners > maximumOverlap])] [
      let searchPatches patches in-radius homeRangeRadius with [ (suitability > thresholdSuitability) and numberOwners < maximumOwners]
      if any? searchPatches [
        ask homeRange [
          remove-from-home
        ]
        move-to one-of searchPatches with-max [suitability]                                                             ;; 1st try: hares move to one of the suitable patches
        set homeRange patches in-radius homeRangeRadius
        ask homeRange
        [
          add-to-home
        ]
        set suithomeRange ((sum [suitability] of homeRange) / homeRangeNumber)                                          ;; the suitability of the homeRange is calculated
        set suithomeRange precision suithomeRange 2
      ]
    ]
    set i i + 1
  ]

  ;; let turtles who did not make it die
  ask matures with [(suithomeRange < thresholdSuitability)]
     [
        ;; get the most updated suitability
         set suithomeRange ((sum [suitability] of homeRange) / homeRangeNumber)                                              ;; the suitability of the homeRange is calculated
         set suithomeRange precision suithomeRange 2
         if suithomeRange < thresholdSuitability                                                                             ;; if there is no suitable home range, die
         [
             ask homeRange
             [
                 remove-from-home
             ]
             die
         ]
     ]
end


to reproduce
  ask turtles with [ status = "female" ] [
       set offspring (12 + random 3) ;; previously hatched females would have the same rate as their mother
       hatch offspring [
           set status "juvenile"
           set color yellow
           set size 10
           set age 0
           set suithomeRange 0
           set homeRange []
       ]
  ]
end

to survive
  ask turtles with [ status != "juvenile" ] [
      if random-float 1 <= mortalityAdult [
         ask turtles-here with [status = "juvenile"] [ die ]
         ask homeRange [
            remove-from-home
         ]
         die
      ]
  ]

  ask turtles with [status = "juvenile"]
     [
         if random-float 1 <= mortalityJuvenile
         [
             die
         ]
     ]
end


to update-view

  ask turtles [ show-turtle ]
         ask patches
         [
             if crop = "wheat" [set pcolor brown]
             if crop = "rape" [set pcolor yellow + 1]
             if crop = "maize" [set pcolor yellow - 3]
             if crop = "barley" [set pcolor orange]
             if crop = "grassland" [set pcolor lime]
             if crop = "pasture" [set pcolor green]
             if crop = "beets" [set pcolor magenta]
             if crop = "alfalfa" [set pcolor violet]
             if crop = "set-aside" [set pcolor white]
             if crop = "rye" [set pcolor yellow + 3]
             if crop = "triticale" [set pcolor green + 2]
             if crop = "silphie" [set pcolor orange + 3]
             if crop = "miscanthus" [set pcolor green - 4]
             if crop = "grass-clover ley" [set pcolor green - 2]
         ]

end
@#$#@#$#@
GRAPHICS-WINDOW
248
11
1027
791
-1
-1
1.928
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
399
-399
0
1
1
1
ticks
30.0

PLOT
7
171
238
448
Population size
year
# hares per km²
0.0
100.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot ( count turtles with [status = \"female\"] + count turtles with [status = \"male\"]) / 16"

PLOT
8
10
236
162
Control
NIL
NIL
0.0
1.0
0.0
1.0
false
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot count turtles"

TEXTBOX
15
27
230
157
plot\n
11
139.9
0

BUTTON
19
32
94
65
NIL
Setup
NIL
1
T
OBSERVER
NIL
S
NIL
NIL
1

BUTTON
96
32
159
65
NIL
Go
NIL
1
T
OBSERVER
NIL
G
NIL
NIL
1

BUTTON
162
32
225
65
Run
Go
T
1
T
OBSERVER
NIL
R
NIL
NIL
1

CHOOSER
21
86
225
131
Scenario
Scenario
"Basic" "Silphie" "Miscanthus" "Grass-clover ley" "Alfalfa" "Set-aside" "Crop richness" "Muenster"
0

PLOT
16
488
216
638
age
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" "plot mean [ age ] of turtles"
PENS
"default" 1.0 0 -16777216 true "" "plot mean [ age ] of turtles"

@#$#@#$#@
# Hare model: ODD protocol

The Hare model was developed by Maria Langhammer and Volker Grimm. For correspondence please contact maria.langhammer@ufz.de.

This model description follows the ODD (Overview, Design concepts, Details) protocol for describing individual-based models (Grimm et al., 2006, Grimm et al., 2010). The model is implemented in NetLogo 6.0.3 (Wilensky, 1999) and used to simulate brown hare population dynamics in bioenergy-driven landscapes.

The design of the model is in parts adopted from Animal Functional Type (AFT) model developed by Scherer et al. (2016). The landscapes were generated using the model developed by Engel et al. (2012), which was further developed by Everaars et al. (2014).

##  Overview

### Purpose

The model aims to evaluate the quality of different agricultural land use patterns for the European brown hare (_Lepus europaeus_). In two representative landscapes, the effectiveness of different mitigation measures in bioenergy-driven landscapes is explored. These measures include alternative energy crops and other measures to increase habitat diversity.

### Entities, state variables and scales

The model includes two types of entities: square grid cells and individuals (hares). Table 1 gives an overview of these entities and their state variables. Hares are characterised by the following key variables and parameters: identity number (_owner_), location (coordinates x and y at the centre of the grid cell they are on), _age, status_ (juvenile, female, male) and home range area (Table 1, Table 2).

**Table 1:** Entities and state variables of the habitat-based hare model.

![Release] (file:Table1_EntitiesAndStateVariables.png)

**Table 2:** Hare parameters of the model with their value or range for the standard parameter set.

![Release] (file:Table2_HareParameters.png)
<sup>a</sup> Another landscape in South Germany, Bavaria, investigated by Ullmann et al. (2018) with an average field size of 3 ha, showed an average hare home range of 19 ha. Based on these data, we interpolated the presumed average value for Germany to 25 ha. This value is comparable to values of 21 ha in Rühe & Hohmann (2004) and 29 ha in Broekhuizen & Maaskamp (1981). A home range of 25 ha corresponds to a radius of 28 grid cells in the model (_Uckermark_), a home range of 55 ha to 42 grid cells (_Germany_).
<sup>b</sup> Hard-coded via algorithm.

Grid cells represent 100 m² and are characterised by their coordinates and the variables assigned to them. Each grid cell is covered by one of 14 crop species determined by the variable _crop_, from which the variables (1) suitability as forage habitat (_foraging_ _FH_), (2) suitability as breeding habitat (_breeding_ _BH_), and (3) crop richness (_richness_ _RC_) are derived. The foraging and breeding values range from 0.0 (not suitable) to 1.0 (very well-suited) and are based on expert knowledge drawn from the literature (Figure 1, Table 3). If we did not find any information about a certain crop, we derived the value of a similar crop (e.g., for cereals) or assumed a mean value of 0.5. An overview of the literature on the ecology of the brown hare, which we have used to assess foraging and breeding preferences, is given in the Supplementary Material.

Suitability as forage habitat, _FH_, specifies the suitability of each crop species as a food source. Suitability as breeding habitat, _BH_, indicates the suitability of the crop species for getting offspring. The value depends on crop density, crop height and management activities. Crop richness, _RC_, indicates the distribution and quantity of crops within the landscape. Many studies show that habitat diversity, in general, including crop richness, has a clear positive effect on hare populations (Tapper & Barnes, 1986, Lewandowski & Nowakowski, 1993, Reichlin et al., 2006, Santilli & Galardi, 2016). Following this, we related the crop richness value to the number of crops in three levels (Table 4). The values were chosen to implement a relationship between overall crop richness in the landscape and habitat suitability. They represent the fact that habitat suitability does not only depend on local features within a habitat, but also on the features of the surrounding landscape. Note that in our simulations, only three values of _RC_ were possible: 0.6 for base landscapes with 10 crop species, 0.8 for landscapes with one additional crop for mitigation, and 1.0 when all 14 crop species listed in Table 5 were present.

The geometric mean of all three variables (_FH_, _BH_, _RC_) results in the habitat suitability value (_SH_) for each individual grid cell (Figure 1):


_SH_=∛(_FH_ × _BH_ × _RC_)

**Figure 1:** Habitat suitability of the base scenarios in _Uckermark_ (left) und _Germany_ (right) as a result of the geometric mean of suitability as forage habitat, suitability as breeding habitat and crop richness. The green colours show habitats above the suitability threshold of 0.5. The grey colours show habitats below the suitability threshold of 0.5. Darker green indicates higher suitability, and lighter grey indicates lower suitability.

![Release] (file:Figure1_HabitatSuitability.png)

**Table 3:** Habitat characteristics of the crop species considered in this study. The suitability values range from 0.0 (not suitable) to 1.0 (very well-suited) and are based on the literature. Values in italics have an intermediate value of 0.5 due to a lack of information to estimate them. Details can be found in the Appendix.

![Release] (file:Table3_HabitatCharacteristics.png)

**Table 4:** Crop richness in terms of the number of crop species in the model landscapes. The crop richness value ranges from 0.0 (not suitable) to 1.0 (very well-suited).

![Release] (file:Table4_CropRichness.png)
AE: Alternative energy plant scenarios
CC: Crop composition scenarios

The hare home ranges in the model landscapes are distributed in a circular shape around the individuals. Females and males have the same home range size in the model, although it can be different in reality. Because the model proceeds in annual steps, juveniles do not have their own home range in the year of birth. In the following year, they are considered sexually mature and are looking for their own home range. The home ranges of several individuals can overlap. However, a grid cell can only be assigned to the home range of a maximum of 10 hares (Figure 2). For each additional hare that marks a cell belonging to its home range, the habitat suitability value of the cell is reduced by 0.02. Both parameters, _homeRangeOverlap_ and _suitabilityReduction_, as well as other unknown parameters (Table 2) were estimated by calibrating the model with the hare counts in the reference landscape in the Uckermark of 5 individuals per 100 ha (data provided by the BioMove Research Training Group DFG GRK 2118/1). They indirectly simulate competition for habitat and avoid unnatural clumping of too many individuals per area.

**Figure 2:** Hare home ranges in the base scenarios in _Uckermark_ (left) und _Germany_ (right). Blue arrows mark males, red arrows indicate females and yellow arrows indicate females with juveniles. The home ranges are represented as circles surrounding the hares. The green colours show habitats above the suitability threshold of 0.5. The grey colours show habitats below the suitability threshold of 0.5. Darker green indicates higher, and lighter grey indicates lower suitability. Note the tracking of habitat suitability by the distribution of hare home ranges and the partly high overlap of home ranges.

![Release] (file:Figure2_HareHomeRanges.png)


In small-scale heterogeneous landscapes, home ranges are smaller than those in landscapes with large monocultures. Following Ullmann et al. (2018), we set the hare home ranges in the Brandenburg scenarios to 55 ha. Another landscape in South Germany, Bavaria, investigated by Ullmann et al. (2018) with an average field size of 3 ha, showed an average hare home range of 19 ha. Based on these data, we interpolated the presumed average value of _Germany_ to be 25 ha. This value is comparable to the values of 21 ha in Rühe & Hohmann (2004) and 29 ha in Broekhuizen & Maaskamp (1981). A home range of 25 ha corresponds to a radius of 28 grid cells (280 m) in the model (_Germany_), i.e., a home range of 55 ha to 42 grid cells (420 m) in _Uckermark_.

A time step in the model represents one year, and simulations are usually run for 80 time steps.

### Process overview and scheduling

In each time step (tick), the following submodels are called in the specified order. The names of the corresponding submodels are printed in italics and are used both in the submodels section and in the program used. A flowchart of the model process is depicted in Figure 3.

First, all hares become one year older, and juveniles become young adults (_aging_). New adults then try to establish a home range (_establish-home range_); they have three attempts to find a grid cell where they can establish a home range with suitability about the _suitabilityThreshold_. If they fail, they die. Adults that reached their maximum age die (_die-of-longevity_). In the next step, the crop species are reassigned to all fields each year (_cultivation_). The selection of the crop species per field depends on the field size and the determined crop proportions for each scenario, i.e., no specific crop rotations are taken into account. However, the proportion of a crop species in the entire landscape remains the same throughout each simulation run for each scenario. Next, the landscape is evaluated from the perspective of the hare (_evaluation_). Depending on the crop species, the variables foraging, breeding and diversity are calculated for each grid cell (_calculate-suitability_). The mean value of all habitat suitability values (_SH_) within the home range describes the general suitability of the home range as a habitat (_calculate-suithomeRange_). In the next step, all hares search within their home range for a suitable position (_search-homeRange_). To do this, the individuals search for suitable patches as start patches within the home range. The search radius is limited to the home range because hares are a sedentary species, and studies show that they do not significantly expand their home range if their energy requirements are not covered (Smith et al., 2005, Bray et al., 2007). The search patches must have a suitability above the _thresholdSuitability_, which indicates the probability of survival and be occupied by 7 individuals maximum. If these requirements are met, the individual moves to the selected patch and installs its home range. Then, the suitability of the entire home range is calculated. If the hare fails three times in finding a new home range, it dies. Failure occurs either through too low habitat quality or too many other individuals within the search radius. Next, all females have 12 to 15 offspring (Marboutin et al., 2003) (_reproduction_). Finally, mortality rates are applied for juveniles and adults (_survival_). Mortality rates reflect the loss due to predation, environmental impacts (e.g., weather conditions) and accidents and are similar to the investigations of Marboutin & Peroux (1995).

Each simulation run ends after 80 years or when the population becomes extinct. The individuals and grid cells are processed in a random order each time step to avoid priority effects.

**Figure 3:** Flowchart of the habitat-based hare model including initialization and sub-models. For a detailed description of each process, see Section 7 _Submodels_.

![Release] (file:Figure3_Flowchart.png)


## Design concepts

### Basic principles

A basic principle of the model is to assign home ranges according to the quality of the habitat (e.g., Carter et al., 2015) in contrast to home range models that are based on tracking data (e.g., Nabe-Nielsen et al., 2014), although in a simplified way by assuming fixed home range sizes. The evaluation of habitat quality takes place within these fixed home ranges.

### Emergence

Hare behaviour is largely imposed, in terms of both home range establishment and selection and demographic rates.

### Adaptation

The hares have to adapt to changing habitat conditions due to a yearly changing crop pattern. Their home ranges are related to the habitat suitability of the arable crops. If they are young adults or their habitat quality is not sufficient, they must disperse to find a more suitable habitat. Therewith, the hares respond to changes in landscape structure and overall hare abundance in an adaptive way.

### Sensing

The hares receive information about the habitat suitability of all cells of their home range. Furthermore, they know their status (juvenile, female or male) and age and are affected by the overall crop richness within the model landscape.

### Interaction

An individual can occupy a new home range only if the total number of individuals on each cell of the respective area is less than 10. This means that the hares compete indirectly for available land. Juvenile hares trying to establish a home range only select grid cells as staring points, which are covered by less than 7 hare home ranges.

### Stochasticity

The configuration and composition of the landscapes is partly random. (1) The agricultural fields are randomly distributed in the landscape by the landscape generator and (2) randomly assigned with crop species according to predefined percentages. (3) The hares are processed in a random order each time step to avoid priority effects. (4) The offspring are 50% female or male. (5) During dispersal, the target patch is randomly selected within the search radius. (6) Females obtain a random number between 12 and 15 offspring. (7) Hare age is random between 1 and 13 in the first time step. All these elements of stochasticity are included to represent natural variation without going into the details of underlying mechanisms.

### Observation

The main output value is the average number of females and males for the last 50 years after the end of the simulation. The first 30 years are discarded to avoid transient effects.

## Initialization

To initialize the model, a landscape derived from a landscape generator written in C++ using Embacadero RAD Studio 12.0 (available upon request) is imported as a text file. The file must contain numerical values in a space-separated table matching the dimensions of the model landscape from the graphical user interface (GUI). The file input workflow is similar to the method presented in Chapter 5 in Railsback & Grimm (2012).

crop species are then distributed to the fields according to the chosen scenario. From each crop species or rather the whole number of crops, the variables (1) suitability as forage habitat, (2) suitability as breeding habitat and (3) crop richness are derived. The habitat suitability is calculated for each grid cell, and the cells are coloured on a green range with the darkest hue marking the best suitability (select “habitat suitability” view). Next, a number of hares are distributed in the landscape according to the variable _initialPopulation_. The default value is 80 hares corresponding to the data of the real landscape in Brandenburg, Germany. Age is assigned randomly between 1 and 13, and gender is either female or male with the same probability. After the first placement, the hares search for a suitable position with sufficient habitat suitability within their home range and claim it. If there is no position available, the hare is removed from the grid.


## Input data

The model does not use any input data that would represent external factors that vary in time.

## Submodels

### Ageing

Because the model follows an annual rhythm, all individuals get one year older in each time step. Juveniles become young adults and search within a radius of 150 grid cells for their own home range (_establish-home range_). If they do not succeed at three, they die. When individuals grow 13 years old, they die (_die-of-longevity_).

### Cultivation

Each cell is assigned a new crop species. Fourteen different crop species are available for selection: alfalfa, barley, beets, grassland, grass-clover ley, maize, miscanthus, oilseed rape, pasture, rye, set-aside, mixed silphie, triticale and wheat. The proportion of a certain crop species in the landscape is defined by a cultivation probability, with the selection of the crop species per field remaining the same throughout each simulation run for each mitigation scenario. Thus, as in reality, crops are assigned to the fields each year, and an evaluation for the hare population takes place. Table 5 shows the cultivation probabilities of all crop species for each scenario.

**Table 5:** The simulated crop proportions for each of the 14 crops and for each scenario. The two base scenarios (UM, GER) match the crop distributions in the reference landscape _Uckermark_ and the average distribution in _Germany_ 2017 for the ten most common crops. For each base scenario, six mitigation strategies are explored: three alternative energy plant scenarios and three crop composition scenarios. For the alternative energy plant scenarios (AE1-AE3), the proportions of mixed silphie, miscanthus and grass-clover ley were increased by 10% in each case. For the first two crop composition scenarios (CC1, CC2), the proportions of alfalfa and set-aside were increased by 10% in each case. Crop composition scenario 3 (CC3) integrates all 14 crops in the landscape. Key changes are displayed in bold.

![Release] (file:Table5_SimulatedCropProportions.png)

### Evaluation

First, the variables (1) suitability as forage habitat (_foraging FH_), (2) suitability as breeding habitat (_breeding BH_) and (3) crop richness (_richness RC_) are derived from each crop species or rather the whole number of crops. Table 3 and Table 4 give an overview of the assessment criteria. The geometric mean of all three variables (_FH, BH, RC_) results in the habitat suitability value (_SH_) for each individual grid cell:

_SH_=∛(_FH × BH × RC_)

Based on this value, the mean habitat suitability of each hare home range is calculated. In the next step, the habitat suitability value of the home range is compared to the habitat suitability threshold of 0.5, which indicates the probability of survival.

### Dispersal

After crop cultivation each year, all adult hares search within their home range for a suitable new position from where to establish a new home range. Therefore, the individual selects a suitable cell in the home range (habitat suitability ≥ 0.5, number of owners ≤ 7) and moves there. Then, it calculates the mean habitat suitability for the prospective home range. If it is sufficient, the hare stays there and establishes its home range. As a consequence, habitat suitability is increased by 0.2 in all grid cells of the original home range and decreased by 0.2 in all cells of the new home range. If the conditions do not apply, the hare searches for a new target cell and tries to find a suitable home range in the same way. If that does not work either, it succeeds in the third try or dies.

Juveniles that mature are searching for a home range within a radius of 150 cells (1.5 km) prior to the assignment of new crop species. Their search radius is larger than that of the adults in order to find suitable grid cells outside the mother’s home range. The other rules applied here are similar to those for adults: they search for a suitable grid cell, defined by suitability and the requirement that no more than nine hares use this cell as part of their home range. Then, if the suitability of the entire home range is, such as with the adults, too low, they try again, but die after the third unsuccessful attempt. Thus, the number of adults alive before reproduction takes place is determined by habitat suitability, which in turn, depends on crop species, field configurations, and the density of conspecifics. These factors affect hare distribution and abundance two times per year, for establishing young adults, and, after new assignments of crops, for established adults.

### Reproduction

Every year sexually mature females get 12 to 15 offspring (Marboutin et al., 2003). The number of offspring is selected at random.

### Survival

The individuals die after a maximum of 13 years of life. They die earlier if the habitat suitability is not sufficient to feed them and they cannot find a new position. Offspring in the first year die when the mother dies. In addition, there is a fixed mortality rate to reflect predation, environmental impacts (e.g., weather conditions) and accidents. The mortality rate for juveniles is 20 % higher than for adults (Marboutin & Peroux, 1995).

## Acknowledgements

M.L. thanks Wiebke Ullmann for the provision of data of the research platform _AgroScapeLab Quillow_ (Agricultural Landscape Laboratory Quillow) established by the Leibniz Centre for Agricultural Landscape Research (ZALF) as well as data gained by the BioMove Research Training Group (DFG GRK 2118/1). Furthermore, M.L. thanks Wiebke Ullmann for helpful knowledge on the ecology and behaviour of the brown hare.


## References

Bray, Y., Devillard, S., Marboutin, E., Mauvy, B. and Péroux, R. (2007): Natal dispersal of European hare in France. Journal of Zoology 273(4): 426-434.

Broekhuizen, S. (1979): Survival in Adult European Hares. Acta Theriologica 24(34): 465-473.

Broekhuizen, S. and Maaskamp, F. (1981): Annual production of young in European hares (Lepus europaeus) in the Netherlands. Journal of Zoology 193(4): 499-516.

Carter, N., Levin, S., Barlow, A. and Grimm, V. (2015): Modeling tiger population and territory dynamics using an agent-based approach. Ecological Modelling 312: 347-362.

Engel, J., Huth, A. and Frank, K. (2012): Bioenergy production and Skylark (Alauda arvensis) population abundance – a modelling approach for the analysis of land-use change impacts and conservation options. GCB Bioenergy 4(6): 713-727.

Everaars, J., Frank, K. and Huth, A. (2014): Species ecology and the impacts of bioenergy crops: an assessment approach with four example farmland bird species. GCB Bioenergy 6(3): 252-264.

Grimm, V., Berger, U., Bastiansen, F., Eliassen, S., Ginot, V., Giske, J., Goss-Custard, J., Grand, T., Heinz, S. K., Huse, G., Huth, A., Jepsen, J. U., Jørgensen, C., Mooij, W. M., Müller, B., Pe’er, G., Piou, C., Railsback, S. F., Robbins, A. M., Robbins, M. M., Rossmanith, E., Rüger, N., Strand, E., Souissi, S., Stillman, R. A., Vabø, R., Visser, U. and DeAngelis, D. L. (2006): A standard protocol for describing individual-based and agent-based models. Ecological Modelling 198(1–2): 115-126.

Grimm, V., Berger, U., DeAngelis, D. L., Polhill, J. G., Giske, J. and Railsback, S. F. (2010): The ODD protocol: A review and first update. Ecological Modelling 221(23): 2760-2768.

Lewandowski, K. and Nowakowski, J. J. (1993): Spatial distribution of brown hare (Lepus europaeus) populations in habitats of various types of agriculture. Acta Theriologica 38(4): 435-442.

Marboutin, E., Bray, Y., Péroux, R., Mauvy, B. and Lartiges, A. (2003): Population dynamics in European hare: breeding parameters and sustainable harvest rates. Journal of Applied Ecology 40(3): 580-591.

Marboutin, E. and Peroux, R. (1995): Survival pattern of European hare in a decreasing population. Journal of Applied Ecology 32(4): 809-816.

Nabe-Nielsen, J., Sibly, R. M., Tougaard, J., Teilmann, J. and Sveegaard, S. (2014): Effects of noise and by-catch on a Danish harbour porpoise population. Ecological Modelling 272: 242-251.

Railsback, S. F. and Grimm, V. (2012). Agent-Based and Individual-Based Modeling: A Practical Introduction. Princeton, New Jersey, Princeton University Press.

Reichlin, T., Klansek, E. and Hackländer, K. (2006): Diet selection by hares (Lepus europaeus) in arable land and its implications for habitat management. European Journal of Wildlife Research 52(2): 109-118.

Rühe, F. and Hohmann, U. (2004): Seasonal locomotion and home-range characteristics of European hares (Lepus europaeus) in an arable region in central Germany. European Journal of Wildlife Research 50(3): 101-111.

Santilli, F. and Galardi, L. (2016): Effect of habitat structure and type of farming on European hare (Lepus europaeus) abundance. Hystrix-Italian Journal of Mammalogy 27(2).

Scherer, C., Jeltsch, F., Grimm, V. and Blaum, N. (2016): Merging trait-based and individual-based modelling: An animal functional type approach to explore the responses of birds to climatic and land use changes in semi-arid African savannas. Ecological Modelling 326: 75-89.

Smith, R. K., Jennings, N. V., Tataruch, F., Hackländer, K. and Harris, S. (2005): Vegetation quality and habitat selection by European haresLepus europaeus in a pastural landscape. Acta Theriologica 50(3): 391-404.

Tapper, S. C. and Barnes, R. F. W. (1986): Influence of Farming Practice on the Ecology of the Brown Hare (Lepus europaeus). Journal of Applied Ecology 23(1): 39-52.

Ullmann, W., Fischer, C., Pirhofer-Walzl, K., Kramer-Schadt, S. and Blaum, N. (2018): Spatiotemporal variability in resources affects herbivore home range formation in structurally contrasting and unpredictable agricultural landscapes. Landscape Ecology 33(9): 1505-1517.

Wilensky, U. (1999): NetLogo (and NetLogo User Manual). Northwestern University, Center for Connected Learning and Computer-Based Modeling.
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

rabbit
false
0
Polygon -7500403 true true 61 150 76 180 91 195 103 214 91 240 76 255 61 270 76 270 106 255 132 209 151 210 181 210 211 240 196 255 181 255 166 247 151 255 166 270 211 270 241 255 240 210 270 225 285 165 256 135 226 105 166 90 91 105
Polygon -7500403 true true 75 164 94 104 70 82 45 89 19 104 4 149 19 164 37 162 59 153
Polygon -7500403 true true 64 98 96 87 138 26 130 15 97 36 54 86
Polygon -7500403 true true 49 89 57 47 78 4 89 20 70 88
Circle -16777216 true false 37 103 16
Line -16777216 false 44 150 104 150
Line -16777216 false 39 158 84 175
Line -16777216 false 29 159 57 195
Polygon -5825686 true false 0 150 15 165 15 150
Polygon -5825686 true false 76 90 97 47 130 32
Line -16777216 false 180 210 165 180
Line -16777216 false 165 180 180 165
Line -16777216 false 180 165 225 165
Line -16777216 false 180 210 210 240

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
NetLogo 6.1.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="Uckermark" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="80"/>
    <metric>count turtles with [status = "female"] + count turtles with [status = "male"]</metric>
    <enumeratedValueSet variable="homeRangeRadiusUM">
      <value value="42"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeRangeRadiusGER">
      <value value="28"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initialPopulation">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mortalityAdult">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="thresholdSuitability">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="View">
      <value value="&quot;OFF&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Landscape">
      <value value="&quot;Uckermark&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Scenario">
      <value value="&quot;Basic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mortalityJuvenile">
      <value value="0.5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="UM_Silphie" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="80"/>
    <metric>count turtles with [status = "female"] + count turtles with [status = "male"]</metric>
    <enumeratedValueSet variable="homeRangeRadiusUM">
      <value value="42"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeRangeRadiusGER">
      <value value="28"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initialPopulation">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mortalityAdult">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="thresholdSuitability">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="View">
      <value value="&quot;OFF&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Landscape">
      <value value="&quot;Uckermark&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Scenario">
      <value value="&quot;Silphie&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mortalityJuvenile">
      <value value="0.5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="UM_Miscanthus" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="80"/>
    <metric>count turtles with [status = "female"] + count turtles with [status = "male"]</metric>
    <enumeratedValueSet variable="homeRangeRadiusUM">
      <value value="42"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeRangeRadiusGER">
      <value value="28"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initialPopulation">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mortalityAdult">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="thresholdSuitability">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="View">
      <value value="&quot;OFF&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Landscape">
      <value value="&quot;Uckermark&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Scenario">
      <value value="&quot;Miscanthus&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mortalityJuvenile">
      <value value="0.5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="UM_Grass-clover" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="80"/>
    <metric>count turtles with [status = "female"] + count turtles with [status = "male"]</metric>
    <enumeratedValueSet variable="homeRangeRadiusUM">
      <value value="42"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeRangeRadiusGER">
      <value value="28"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initialPopulation">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mortalityAdult">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="thresholdSuitability">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="View">
      <value value="&quot;OFF&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Landscape">
      <value value="&quot;Uckermark&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Scenario">
      <value value="&quot;Grass-clover ley&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mortalityJuvenile">
      <value value="0.5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="UM_Alfalfa" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="80"/>
    <metric>count turtles with [status = "female"] + count turtles with [status = "male"]</metric>
    <enumeratedValueSet variable="homeRangeRadiusUM">
      <value value="42"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeRangeRadiusGER">
      <value value="28"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initialPopulation">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mortalityAdult">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="thresholdSuitability">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="View">
      <value value="&quot;OFF&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Landscape">
      <value value="&quot;Uckermark&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Scenario">
      <value value="&quot;Alfalfa&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mortalityJuvenile">
      <value value="0.5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="UM_Set-aside" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="80"/>
    <metric>count turtles with [status = "female"] + count turtles with [status = "male"]</metric>
    <enumeratedValueSet variable="homeRangeRadiusUM">
      <value value="42"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeRangeRadiusGER">
      <value value="28"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initialPopulation">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mortalityAdult">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="thresholdSuitability">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="View">
      <value value="&quot;OFF&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Landscape">
      <value value="&quot;Uckermark&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Scenario">
      <value value="&quot;Set-aside&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mortalityJuvenile">
      <value value="0.5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="UM_Diversity" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="80"/>
    <metric>count turtles with [status = "female"] + count turtles with [status = "male"]</metric>
    <enumeratedValueSet variable="homeRangeRadiusUM">
      <value value="42"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeRangeRadiusGER">
      <value value="28"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initialPopulation">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mortalityAdult">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="thresholdSuitability">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="View">
      <value value="&quot;OFF&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Landscape">
      <value value="&quot;Uckermark&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Scenario">
      <value value="&quot;Crop diversity&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mortalityJuvenile">
      <value value="0.5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Germany" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="80"/>
    <metric>count turtles with [status = "female"] + count turtles with [status = "male"]</metric>
    <enumeratedValueSet variable="homeRangeRadiusUM">
      <value value="42"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeRangeRadiusGER">
      <value value="28"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initialPopulation">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mortalityAdult">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="thresholdSuitability">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="View">
      <value value="&quot;OFF&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Landscape">
      <value value="&quot;Germany&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Scenario">
      <value value="&quot;Basic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mortalityJuvenile">
      <value value="0.5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="GER_Silphie" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="80"/>
    <metric>count turtles with [status = "female"] + count turtles with [status = "male"]</metric>
    <enumeratedValueSet variable="homeRangeRadiusUM">
      <value value="42"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeRangeRadiusGER">
      <value value="28"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initialPopulation">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mortalityAdult">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="thresholdSuitability">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="View">
      <value value="&quot;OFF&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Landscape">
      <value value="&quot;Germany&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Scenario">
      <value value="&quot;Silphie&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mortalityJuvenile">
      <value value="0.5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="GER_Miscanthus" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="80"/>
    <metric>count turtles with [status = "female"] + count turtles with [status = "male"]</metric>
    <enumeratedValueSet variable="homeRangeRadiusUM">
      <value value="42"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeRangeRadiusGER">
      <value value="28"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initialPopulation">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mortalityAdult">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="thresholdSuitability">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="View">
      <value value="&quot;OFF&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Landscape">
      <value value="&quot;Germany&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Scenario">
      <value value="&quot;Miscanthus&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mortalityJuvenile">
      <value value="0.5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="GER_Grass-clover" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="80"/>
    <metric>count turtles with [status = "female"] + count turtles with [status = "male"]</metric>
    <enumeratedValueSet variable="homeRangeRadiusUM">
      <value value="42"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeRangeRadiusGER">
      <value value="28"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initialPopulation">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mortalityAdult">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="thresholdSuitability">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="View">
      <value value="&quot;OFF&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Landscape">
      <value value="&quot;Germany&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Scenario">
      <value value="&quot;Grass-clover ley&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mortalityJuvenile">
      <value value="0.5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="GER_Alfalfa" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="80"/>
    <metric>count turtles with [status = "female"] + count turtles with [status = "male"]</metric>
    <enumeratedValueSet variable="homeRangeRadiusUM">
      <value value="42"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeRangeRadiusGER">
      <value value="28"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initialPopulation">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mortalityAdult">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="thresholdSuitability">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="View">
      <value value="&quot;OFF&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Landscape">
      <value value="&quot;Germany&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Scenario">
      <value value="&quot;Alfalfa&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mortalityJuvenile">
      <value value="0.5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="GER_Set-aside" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="80"/>
    <metric>count turtles with [status = "female"] + count turtles with [status = "male"]</metric>
    <enumeratedValueSet variable="homeRangeRadiusUM">
      <value value="42"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeRangeRadiusGER">
      <value value="28"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initialPopulation">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mortalityAdult">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="thresholdSuitability">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="View">
      <value value="&quot;OFF&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Landscape">
      <value value="&quot;Germany&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Scenario">
      <value value="&quot;Set-aside&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mortalityJuvenile">
      <value value="0.5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="GER_Diversity" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="80"/>
    <metric>count turtles with [status = "female"] + count turtles with [status = "male"]</metric>
    <enumeratedValueSet variable="homeRangeRadiusUM">
      <value value="42"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeRangeRadiusGER">
      <value value="28"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initialPopulation">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mortalityAdult">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="thresholdSuitability">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="View">
      <value value="&quot;OFF&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Landscape">
      <value value="&quot;Germany&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Scenario">
      <value value="&quot;Crop diversity&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mortalityJuvenile">
      <value value="0.5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="sensitiv_threshold" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="80"/>
    <metric>count turtles with [status = "female"] + count turtles with [status = "male"]</metric>
    <enumeratedValueSet variable="homeRangeRadiusUM">
      <value value="42"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeRangeRadiusGER">
      <value value="28"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initialPopulation">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mortalityAdult">
      <value value="0.3"/>
    </enumeratedValueSet>
    <steppedValueSet variable="thresholdSuitability" first="0" step="0.1" last="1"/>
    <enumeratedValueSet variable="View">
      <value value="&quot;OFF&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Landscape">
      <value value="&quot;Uckermark&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Scenario">
      <value value="&quot;Basic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mortalityJuvenile">
      <value value="0.5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="sensitiv_homerange" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="80"/>
    <metric>count turtles with [status = "female"] + count turtles with [status = "male"]</metric>
    <steppedValueSet variable="homeRangeRadius" first="10" step="5" last="50"/>
    <enumeratedValueSet variable="initialPopulation">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mortalityAdult">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="thresholdSuitability">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="View">
      <value value="&quot;OFF&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Landscape">
      <value value="&quot;Uckermark&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Scenario">
      <value value="&quot;Basic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mortalityJuvenile">
      <value value="0.5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Vorlage mit crops" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="80"/>
    <metric>count turtles with [status = "female"] + count turtles with [status = "male"]</metric>
    <metric>precision (100 * (count patches with [crop = "wheat"]) / 160000) 1</metric>
    <metric>precision (100 * (count patches with [crop = "rape"]) / 160000) 1</metric>
    <metric>precision (100 * (count patches with [crop = "maize"]) / 160000) 1</metric>
    <metric>precision (100 * (count patches with [crop = "barley"]) / 160000) 1</metric>
    <metric>precision (100 * (count patches with [crop = "grassland"]) / 160000) 1</metric>
    <metric>precision (100 * (count patches with [crop = "pasture"]) / 160000) 1</metric>
    <metric>precision (100 * (count patches with [crop = "beets"]) / 160000) 1</metric>
    <metric>precision (100 * (count patches with [crop = "alfalfa"]) / 160000) 1</metric>
    <metric>precision (100 * (count patches with [crop = "set-aside"]) / 160000) 1</metric>
    <metric>precision (100 * (count patches with [crop = "rye"]) / 160000) 1</metric>
    <metric>precision (100 * (count patches with [crop = "triticale"]) / 160000) 1</metric>
    <metric>precision (100 * (count patches with [crop = "silphie"]) / 160000) 1</metric>
    <metric>precision (100 * (count patches with [crop = "miscanthus"]) / 160000) 1</metric>
    <metric>precision (100 * (count patches with [crop = "grass-clover ley"]) / 160000) 1</metric>
    <enumeratedValueSet variable="homeRangeRadiusUM">
      <value value="42"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homeRangeRadiusGER">
      <value value="28"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initialPopulation">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mortalityAdult">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="thresholdSuitability">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="View">
      <value value="&quot;OFF&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Landscape">
      <value value="&quot;Uckermark&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Scenario">
      <value value="&quot;Miscanthus&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mortalityJuvenile">
      <value value="0.5"/>
    </enumeratedValueSet>
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
