extensions [csv profiler]   ;; Used extensions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;      VARIABLE DECLARATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
breed [entities entity]
breed [agencies agency]
breed [inspections inspection]

entities-own [
   e-lst-resources      ;;  list with resource needs of entity
   e-avrg-resources     ;;  entity's average resource requirements
   e-lst-history        ;;  list with inspection history known to entity
   e-lst-influence      ;;  list with influence of inspection history
   e-lst-insp-risk      ;;  list with variables for inspection risk calculation
   e-lst-compliance     ;;  list with compliance or non-compliance with the rules
   e-risk-appetite      ;;  entity's risk appetite
]

agencies-own [
   ;;;;;;;;;;;;;;;;;;;;;;;  INSPECTION-RELATED VARIABLES
   a-i-nr-of-inspcts    ;;  number of inspections in one turn

   ;;;;;;;;;;;;;;;;;;;;;;;  Random entity inspection strategy related variables
   a-rnd-lst            ;;  list of entites for random inspection

   ;;;;;;;;;;;;;;;;;;;;;;;  Cyclical inspection strategy related variables
   a-cyc-lst-entity     ;;  list of entites for cyclic inspection
   a-cyc-lst-rule       ;;  list of rules for cyclic inspection
   a-cyc-count-entity   ;;  counter of entities for cyclic inspection
   a-cyc-count-rule     ;;  counter of rules for cyclic inspection
   a-cyc-slide          ;;  "slide" of cyclic inspections

   ;;;;;;;;;;;;;;;;;;;;;;;  Stohastic universal sampling (SUS) strategy related variables
   a-sus-sum-resources  ;;  Sum of all resource requirements
   a-sus-window         ;;  SUS inspection window

]

inspections-own [
  i-insp-entity         ;;  inspected entity
  i-insp-rule           ;;  inspected rule
  i-insp-result         ;;  inspection result
]

globals [
   ;;;;;;;;;;;;;;;;;;;;;;;  INSPECTION-RELATED GLOBAL VARIABLES
   g-i-lst-resources    ;;  list with resource needs assumed by the inspection agency
   g-i-history          ;;  global history of all remembered inspections

   ;;;;;;;;;;;;;;;;;;;;;;;  LEARNING STRATEGIES RELATED VARIABLES
   g-memory-turns       ;;  how many turns are remembered
   g-lst-correction     ;;  list of discounting correction factors

   ;;;;;;;;;;;;;;;;;;;;;;;  REPORTER VARIABLES
   rp-violations        ;;  total nr. of violations
   rp-compliance        ;;  total nr. of compliance
   rp-trn-violations    ;;  nr. of violations in each turn
   rp-trn-compliance    ;;  nr. of compliance in each turn
   rp-trn-inspct-compli ;;  nr. of inspections with the result "compliance" in each turn
   rp-trn-inspct-viol   ;;  nr. of inspections with the result "violation" in each turn
   rp-inspct-entities   ;;  inspected entities
   rp-inspct-rules      ;;  inspected rules
   rp-entity-violation  ;;  violations per entity in each turn
   rp-rule-violation    ;;  violations per rule in each turn

]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;      HELPER FUNCTIONS AND PROCEDURES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;; count occurences of "x" in the list "the-list"
to-report occurrences [ x the-list ]
  report length filter [? = x] the-list
end

;;;;;; count occurences of "x" (+ or -) in the list "the-list"
to-report abs-occurrences [ x the-list ]
  report length filter [abs (?) = x ] the-list
end

;;;;;; count the number of elements in the list "the-list" _greater_ than "x"
to-report greater [ x the-list ]
  report length filter [? > x] the-list
end

;;;;;; count the number of elements in the list "the-list" _smaller_ than x
to-report smaller [ x the-list ]
  report length filter [? < x] the-list
end

;;;;;; replace element on position "first-dim", "second-dim" of a 2-dimensional
;;;;;; list ("list-name") with a new element ("new-value")
to-report replace-elem [ first-dim second-dim new-value list-name ]
  let out-list [0]

  set out-list (replace-item first-dim list-name
                  (replace-item second-dim (item first-dim list-name) new-value) )
  report out-list
end

;;;;;; Simulation output into .CSV file placed in the model's home directory
to out-prnt [ the-list ]
  file-open "output.csv"
  foreach the-list [
    file-type ?
  ]
  file-print " "
  file-close
end

;;;;;; Debug output into .CSV file placed in the model's home directory
to debug-prnt [ the-list ]
  file-open "debug.txt"
  foreach the-list [
    file-type ?
  ]
  file-print " "
  file-close
end

;;;;;; (visual) clean-up of inspections (agents)
to clean-up
  ask inspections [
    set color white
    home
  ]
end

;;;;;; profiler function
to profile
  profiler:reset
  profiler:start
  setup
  repeat 100 [go]   ;; number of times the "go" is run
  profiler:stop
  let _fname "profiler-report.log"  ;; output file
  carefully [file-delete _fname] []
  file-open _fname
  file-print profiler:report
  file-close
end

;;;;;;; Validation function
to validate

  let tick-nr 12      ;; set tick number
  let l-lst-in []

  setup
  set g-i-history n-values number-of-entities [0]
  repeat tick-nr [go]  ;; number of repetitions in validation

  ;;;;; Model: IT bank
  ask entities [
    set l-lst-in reverse e-lst-history   ;; list now starts from tick 1
    foreach but-first l-lst-in [           ;; For each tick:
    ]
  ]

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;      MAKE ENTITIES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
to make-entities

  if debug [ debug-prnt (list "$020# ==> make-entities "  ) ]

  create-entities number-of-entities [
    move-to one-of patches with [not any? turtles-here]      ;; randomly assigning entities
    set shape "pentagon"                                     ;; entities are shaped as pentagons
    set color white	                                         ;; default entity color is white
    set label-color black                                    ;; with black labels

    set e-lst-history [0]
    set e-lst-insp-risk []
    ;; setting entity's resource requirements: global +/- max-deviation-resources variation
    set e-lst-resources map [ ? + (? * (precision (random-float (max-deviation-resources * 2) -
                              max-deviation-resources ) 2 )) ] g-i-lst-resources
    set e-avrg-resources (sum (e-lst-resources) / number-of-rules )

    ifelse (risk-exp = TRUE) [  ;; setting entity's risk appetite
      set e-risk-appetite ( precision (random-exponential default-risk-attitude ) 2 ) + 0.0001 ;;  exponential
    ]
    [  ;; risk-exp = FALSE : uniform risk distribution
      set e-risk-appetite default-risk-attitude + (default-risk-attitude *
              precision ((random-float ( 2 * max-risk-attitude-deviation) -
              max-risk-attitude-deviation ) / 100) 2 )
    ]

    if debug [ debug-prnt (list "$020# entity=" who "  e-risk-appetite="  e-risk-appetite) ]

    set e-lst-compliance n-values number-of-rules [1]
    set e-lst-influence n-values g-memory-turns [0]
    set label who

  ]
  if debug [ debug-prnt (list "$030# <== make-entities ") ]
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;      MAKE AGENCY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
to make-agency

  if debug [ debug-prnt (list "$040# ==> make-agency ") ]
  create-agencies 1 [
    set shape "circle"
    set color black

    set a-cyc-lst-entity n-values number-of-entities [?]  ;; list of all entites
;    if debug [ debug-prnt (list "$042# a-cyc-lst-entity " a-cyc-lst-entity) ]
    set a-cyc-lst-rule n-values number-of-rules [?]       ;; list of all rules
    set a-i-nr-of-inspcts (number-of-entities * number-of-rules * inspectors-capacity)  ;; number of inspectors

    if (type-of-inspection-selection = "Cycle") [
      set a-cyc-lst-entity (shuffle a-cyc-lst-entity)  ;; reshuffling list of entities
      set a-cyc-lst-rule (shuffle a-cyc-lst-rule)      ;; reshuffling list of rules
      set a-cyc-count-entity 0
      set a-cyc-count-rule 0
      if (rules-inspected-in-one-cycle > number-of-rules)
        [ set rules-inspected-in-one-cycle number-of-rules ]   ;; max rules inspected in one cycle = all rules
      set a-cyc-slide rules-inspected-in-one-cycle
    ]

    ;; if "Stohastic universal sampling", calculate size of the "window" and make random selection
    ;; in inteval [0, window]
    if (type-of-inspection-selection = "Stohastic universal sampling") [
      set a-sus-sum-resources sum g-i-lst-resources
      set a-sus-window ( (a-sus-sum-resources * number-of-entities) /
                         floor (number-of-rules * number-of-entities * inspectors-capacity) )
      if debug [ debug-prnt (list "$045# a-sus-sum-resources=" a-sus-sum-resources "  a-sus-window=" a-sus-window  ) ]
    ]

    hatch-inspections a-i-nr-of-inspcts [    ;; hatching all inspections
      set shape "person"
      set color white
    ]
  ]
  if debug [ debug-prnt (list "$050# <== make-agency ") ]

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;      PLAY ENTITIES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
to play-entities

  let l-assess-insp-prob 0        ;; Assessed inspection probability
  let l-violations 0              ;; Reporting variable: number of violations in one turn
  let l-compliance 0              ;; Reporting variable: number of compliant rules in one turn
  let l-aggr-violations 0         ;; Reporting variable: aggregate number of violations
  let l-aggr-compliance 0         ;; Reporting variable: aggregate number of compliant rules
  let l-lst-inspections []        ;; History of inspections in one time-step
  let l-lst-results []            ;; Results of discounting
  let l-steps 0                   ;; Step counter
  let l-V 0                       ;; Temp variable for assessment of inspection probability
  let l-A 0                       ;; Temp variable for assessment of inspection probability
  let l-counter 0                 ;; Counter variable
  let l-sum-discount 0            ;; Sum of all discounting factors
  let l-lst-weighted-insp-prob [] ;; List of stackelberg-weighted inspection probabilities
  let l-resources-avg 0           ;; Average resource cost
  let l-insp-riskiness 1          ;; Calculated riskiness of inspections
  let l-detect-noncomp 0          ;; Detected non-compliance - for riskiness of inspections
  let l-detect-comp 0             ;; Detected compliance - for riskiness of inspections
  let l-all-noncomp 0             ;; All non-compliant rules - for riskiness of inspections
  let l-riskiness 0               ;; Assessed inspection riskiness of the last tick


  set rp-entity-violation n-values number-of-entities [0]  ;; initialization of reporter of violations
  set rp-rule-violation []                                 ;; initialization of reporter

  if debug [ debug-prnt (list "$060# ==> play-entities,         ticks=" ticks) ]

  ;; preparation for discounting
  ask one-of entities [                                             ;; calling this only once because this calculation applies to all entities
    set l-steps ( (length e-lst-history) - 1 )                      ;; setting number of historic steps
    if (g-memory-turns < l-steps) [ set l-steps g-memory-turns ]    ;; that are remembered
    set l-sum-discount (sum (sublist g-lst-correction 0 l-steps) )
  ]

  ;; setting values of compliance/noncompliance for all rules by comparing costs of compliance
  ;; and product of inspection probability and punishment
  ask entities [

    ;;;;;;;;;;; calculation of assessed inspection probability

    ifelse (l-steps = 0) [  ;; first step
      set l-assess-insp-prob (inspectors-capacity * (1 / e-risk-appetite) ) ;; Initially, inspection probability = capacity * (1 / risk appetite)
      if debug [ debug-prnt (list "$083#  l-assess-insp-prob="  l-assess-insp-prob ) ]
    ]
    [  ;; (l-steps > 0)
      set l-V 0
      set l-lst-inspections (item 0 e-lst-history)  ;; results of the previous turn
      if debug [ debug-prnt (list "$084#  l-lst-inspections="  l-lst-inspections "  l-steps="  l-steps) ]

      ifelse (learning-mechanism = "Reinforcement learning") [
        set l-A ( ( occurrences -2 l-lst-inspections ) -
                  ( occurrences -1 l-lst-inspections ) ) / number-of-rules
      ]
      [  ;; else: (learning-mechanism = "Fictitious play")
        set l-A ( ( abs-occurrences 2 l-lst-inspections )  / number-of-rules ) ;; counts -2 (inspected violations) and 2 (inspected compliance)
      ]

      set e-lst-influence (fput l-A e-lst-influence)  ;; updating the list of historic influence

      if debug [ debug-prnt (list "$085#  l-A="  l-A "  e-lst-influence="  e-lst-influence) ]

      if (length e-lst-influence > g-memory-turns)
        [ set e-lst-influence (but-last e-lst-influence) ]                  ;; shortening of historic influence list if too long
      set l-lst-results ( map [?1 * ?2] e-lst-influence g-lst-correction )  ;; mapping historic results with correction factors
      set l-V ( (sum l-lst-results ) / l-sum-discount )                     ;; calculating final objective assessment of inspection probability

      if debug [ debug-prnt (list "$086#  e-lst-influence="  e-lst-influence "  g-lst-correction="  g-lst-correction "  l-lst-results="  l-lst-results "  l-V="  l-V) ]

      ;;;;;;;;; Calculation of inspection riskiness

      set l-all-noncomp (smaller 0 l-lst-inspections)   ;; number of non-compliant rules

      ifelse (l-all-noncomp = 0)                                ;;  If there are no non-compliant rules,
      [ set l-riskiness 0 ]                                     ;;  no influence on inspection riskiness
      [                                                         ;;  Otherwise, l-all-noncomp > 0
        set l-detect-noncomp (occurrences -2 l-lst-inspections) ;;  Detected non-compliance
        set l-detect-comp (occurrences  2 l-lst-inspections)    ;;  Detected compliance

        ifelse ((l-detect-noncomp + l-detect-comp) = 0 )        ;; if there was no inspection
          [ set l-riskiness 0 ]
          [ ifelse (l-detect-noncomp = 0)                       ;; Otherwise, if there are no detected non-compliant rules
            [ set l-riskiness ( (number-of-rules - l-all-noncomp) /
                                                number-of-rules ) ^ l-detect-comp ]     ;; Combinatorics approach
            [ set l-riskiness (l-detect-noncomp * number-of-rules) /
                              ( (l-detect-noncomp + l-detect-comp) * l-all-noncomp ) ]  ;; Comparison approach
          ]
      ]

      set e-lst-insp-risk (fput l-riskiness e-lst-insp-risk)

      if (length e-lst-insp-risk > 5)
        [ set e-lst-insp-risk but-last e-lst-insp-risk ]

      ifelse (sum e-lst-insp-risk > 0)
        [ set l-insp-riskiness ( mean (filter [? > 0] e-lst-insp-risk ) ) ]  ;; product of all elements of e-lst-insp-risk > 0
        [ set l-insp-riskiness 1 ]

      if debug [ debug-prnt (list "$088#  who=" who " l-riskiness=" l-riskiness " l-detect-noncomp=" l-detect-noncomp " l-detect-comp=" l-detect-comp " l-all-noncomp=" l-all-noncomp " e-lst-insp-risk="  e-lst-insp-risk "  l-insp-riskiness="  l-insp-riskiness ) ]

      ifelse (learning-mechanism = "Reinforcement learning")
        [ set l-assess-insp-prob ( ( l-assess-insp-prob * (1 + l-A) ) * (1 / e-risk-appetite) * l-insp-riskiness) ] ;; correction of assessed inspection  probability
        [ set l-assess-insp-prob ( l-V * (1 / e-risk-appetite) * l-insp-riskiness) ]                                ;; else: (learning-mechanism = "Fictitious play")

      if debug [ debug-prnt (list "$090#  l-V=" l-V " (1 / e-risk-appetite)=" (1 / e-risk-appetite) " l-insp-riskiness=" l-insp-riskiness " l-assess-insp-prob="  l-assess-insp-prob ) ]
    ]

    if debug [ debug-prnt (list "$0A0# ** Entity=" who "  l-steps="  l-steps "  l-V=" l-V "  l-assess-insp-prob=" l-assess-insp-prob ) ]

    ;; if cost of compliance is less than punishment * assessed inspection probability: Comply [1], otherwise, violate [-1]
    ifelse (stackelberg-aware = TRUE)
    [

      set l-lst-weighted-insp-prob map [(? / e-avrg-resources) * l-assess-insp-prob] e-lst-resources
      set e-lst-compliance ( map [ ifelse-value (?1 < punishment-size * ?2)
                                           [1] [-1] ] e-lst-resources l-lst-weighted-insp-prob )
      if debug [ debug-prnt (list "$0A3#  Stackelberg:  e-avrg-resources="  e-avrg-resources "  e-lst-resources=" e-lst-resources "  l-lst-weighted-insp-prob=" l-lst-weighted-insp-prob "  e-lst-compliance=" e-lst-compliance) ]
    ]
    [   ;; (stackelberg-aware = FALSE)
      set e-lst-compliance map [ ifelse-value (? < punishment-size * l-assess-insp-prob)
                                                       [1] [-1] ] e-lst-resources
      if debug [ debug-prnt (list "$0A5#  non-Stackelberg:  e-lst-resources=" e-lst-resources "  e-lst-compliance=" e-lst-compliance) ]
    ]
    set e-lst-history fput e-lst-compliance e-lst-history
;    if debug [ debug-prnt (list "$0B0# ** Entity=" who "  e-lst-history="  e-lst-history) ]
;    if (ticks mod 5 = 0) [ if print-values [ out-prnt ( list ticks ";" who ";" l-assess-insp-prob ";" punishment-size ";" e-lst-resources ";" e-lst-compliance) ] ]
;    if print-values [ out-prnt ( list ticks ";" type-of-inspection-selection ";" who ";" l-assess-insp-prob ";" punishment-size ";" e-lst-resources ";" e-lst-compliance) ]

    ;;;;;;;;;;;;;;;;;;;;;  reporting variables
    set l-violations (occurrences -1 e-lst-compliance)
    set l-compliance (occurrences  1 e-lst-compliance)
    set l-aggr-violations (l-aggr-violations + l-violations)
    set l-aggr-compliance (l-aggr-compliance + l-compliance)

    set rp-entity-violation (replace-item who rp-entity-violation l-violations) ;; set who variables

    set l-counter 0
    repeat number-of-rules [
      if (item l-counter e-lst-compliance) = -1
        [ set rp-rule-violation fput l-counter rp-rule-violation ]
      set l-counter (l-counter + 1)
    ]

    ;; display
    set color ( ((number-of-rules - l-violations) / number-of-rules) * 4 + 135 )

  ]

  if debug [ debug-prnt (list "$0C0# rp-entity-violation=" rp-entity-violation) ]

  set rp-trn-violations l-aggr-violations
  set rp-violations (rp-violations + rp-trn-violations)
  set rp-trn-compliance l-aggr-compliance
  set rp-compliance (rp-compliance + rp-trn-compliance)

  if debug [ debug-prnt (list "$0D0# <== play-entities") ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;      PLAY AGENCY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
to play-agency
  let l-entity-choice -1     ;; entity chosen for insepection
  let l-rule-choice -1       ;; rule chosen for insepection
  let l-lst-rule []          ;; list of rules for tracking inspection history
  let l-lst-ent []           ;; list of entites for tracking inspection history
  let l-window 0             ;; SUS: selection window
  let l-selection 0          ;; SUS: random selection in inteval [0, window]
  let l-remainder 0          ;; SUS: remainder of selection / sum-resources
  let l-counter 0            ;; counter variable
  let l-counter-two 0        ;; counter variable
  let l-sum-tmp 0            ;; temporary sum
  let l-asses-compl 0        ;; assessed compliance
  let l-lst-entity-rule []   ;; Random: list of all possible entity-rule combinations
  let l-rnd-lst []           ;; Random: list of entites and rules for random inspection

  if debug [ debug-prnt (list "$0E0# ==> play-agency") ]

  ;; setting 2D list for tracking history (size: number-of-entities * number-of-rules)
  set l-lst-rule n-values number-of-rules [0]
  set l-lst-ent n-values number-of-entities [l-lst-rule]

;  set l-t-lst n-values number-of-entities [0]

  set rp-trn-inspct-compli 0
  set rp-trn-inspct-viol 0


  ask agencies [
    if (type-of-inspection-selection = "Stohastic universal sampling") [
      ;; if "Stohastic universal sampling", calculate size of the "window" and make random selection
      ;; in inteval [0, window]
      set l-selection random-float a-sus-window  ]
    if (type-of-inspection-selection = "Random") [
      ;; if "Random", determine a-i-nr-of-inspcts (number of inspections) random pairs of entites and rules for inspection
      set l-lst-entity-rule (n-values (number-of-entities * number-of-rules) [?] )   ;; helper list with (number-of-entities * number-of-rules) sequential elements
      set l-rnd-lst n-of a-i-nr-of-inspcts l-lst-entity-rule                         ;; random selection of rules and entities for inspection
      if debug [ debug-prnt (list "$0F1# l-lst-entity-rule=" l-lst-entity-rule "  l-rnd-lst=" l-rnd-lst )]
    ]
    if (type-of-inspection-selection = "Random entity") [
      ;; if "Random entity", determine (number of inspections) / (number of rules) random entites for inspection
      ;; i.e. for selected entity all rules will be inspected
      set l-lst-entity-rule (n-values (number-of-entities) [?] )                                  ;; helper list with number-of-entities sequential elements
      set l-sum-tmp ( number-of-entities * inspectors-capacity )
      ifelse (l-sum-tmp = (floor l-sum-tmp ))                            ;; If all rules for given entities can be inspected
        [ set a-rnd-lst n-of (l-sum-tmp) l-lst-entity-rule ]             ;; random selection of (floor) entities for inspection
        [ set a-rnd-lst n-of (floor (l-sum-tmp) + 1) l-lst-entity-rule ] ;; random selection of (Floor + 1) entities for inspection
      set a-rnd-lst shuffle a-rnd-lst                                    ;; shuffle selected entites
      set l-sum-tmp 0
      if debug [ debug-prnt (list "$0F2# l-lst-entity-rule=" l-lst-entity-rule "  a-rnd-lst=" a-rnd-lst )]
    ]
  ]

  ask inspections [
;    if debug [ debug-prnt (list "$0G0# -- Inspection=" who ) ]
    ;; ======================================= Inspection preparation =======================================

    ifelse (type-of-inspection-selection = "Random") [    ;; if random inspection strategy, inspections decide independently
    ;;---------------------------------------- Random begin ----------------------------------------
      set i-insp-entity floor ( (item l-counter l-rnd-lst) / number-of-rules ) ;; select entity
      set i-insp-rule (item l-counter l-rnd-lst) mod number-of-rules           ;; and the rule
      set l-counter (l-counter + 1)
      if debug [ debug-prnt (list "$0F2# i-insp-entity=" i-insp-entity "  i-insp-rule=" i-insp-rule ) ]
    ;;---------------------------------------- Random end ------------------------------------------
    ]
    [                                                     ;; otherwise, they have to ask the agency
      ask agencies [

        if (type-of-inspection-selection = "Random entity") [
          ;;---------------------------------------- Random entity begin ---------------------------------
          set l-entity-choice (item l-counter a-rnd-lst)  ;; selected entity
          set l-rule-choice l-counter-two                 ;; selected rule
          set l-counter-two (l-counter-two + 1)           ;; incrementing counter
          if (l-counter-two = number-of-rules) [          ;; if the last rule (number-of-rules - 1) was inspected:
            set l-counter (l-counter + 1)                 ;; go to next entity
            set l-counter-two 0                           ;; reset rule counter
          ]
          if debug [ debug-prnt (list "$0H0# l-counter=" l-counter "  l-counter-two=" l-counter-two "  l-entity-choice=" l-entity-choice "  l-rule-choice=" l-rule-choice) ]
          ;;---------------------------------------- Random entity end -----------------------------------
        ]
        if (type-of-inspection-selection = "Cycle") [
          ;;---------------------------------------- Cyclical begin --------------------------------------

          if (a-cyc-count-rule = number-of-rules) [                ;; if all rules have been inspected
            ifelse (a-cyc-count-entity = number-of-entities - 1) [              ;; and the last entity is currently being inspected
              set a-cyc-count-rule 0                                            ;; reset rule counter
              set a-cyc-count-entity 0                                          ;; reset entity counter
              set a-cyc-slide rules-inspected-in-one-cycle                      ;; reset slide
            ]
            [                                                      ;; otherwise (current entity is not the last)
              set a-cyc-count-rule (a-cyc-slide - rules-inspected-in-one-cycle) ;; move to the first rule in current slide
              set a-cyc-count-entity (a-cyc-count-entity + 1)                   ;; and move to the next entity
            ]
          ]

          if (a-cyc-count-rule = a-cyc-slide) [                    ;; if at slide,
            set a-cyc-count-entity (a-cyc-count-entity + 1)                     ;; move to the next entity

            ifelse (a-cyc-count-entity = number-of-entities) [                  ;; if previusly inspected entitiy is the last entity,
              set a-cyc-count-entity 0                                          ;; go to the first etity and
              set a-cyc-slide (a-cyc-slide + rules-inspected-in-one-cycle)      ;; increase slide
            ]
            [                                                                   ;; otherwise (current entity is not the last)
              set a-cyc-count-rule (a-cyc-slide - rules-inspected-in-one-cycle) ;; move to the first rule in current slide
            ]
          ]

    ;      if debug [ debug-prnt (list "$0H4# slide=" a-cyc-slide "  entity=" a-cyc-count-entity "  rule=" a-cyc-count-rule ) ]

          set l-entity-choice (item a-cyc-count-entity a-cyc-lst-entity)       ;; set inspection choice: entity
          set l-rule-choice (item a-cyc-count-rule a-cyc-lst-rule)             ;; set inspection choice: rule

          set a-cyc-count-rule (a-cyc-count-rule + 1)                          ;; move to the next rule

        ]
        ;;---------------------------------------- Cyclical end ----------------------------------------

        if (type-of-inspection-selection = "Stohastic universal sampling") [
        ;;---------------------------------------- SUS begin -------------------------------------------
          set l-entity-choice floor (l-selection / a-sus-sum-resources)        ;; chosen entity is whole part of selection / sum-resources
          set l-remainder (remainder l-selection a-sus-sum-resources)          ;; while chosen entity is remainder of selection / sum-resources

          set l-sum-tmp 0
          set l-counter 0
          while [l-sum-tmp < l-remainder] [                           ;; while temporary sum is smaller than reminder
            set l-sum-tmp (l-sum-tmp + item l-counter g-i-lst-resources)       ;; add resource requirements of next rule to temporary sum
            set l-counter (l-counter + 1)                                      ;; and increase conuter
          ]                                                           ;; when while loop ends, temporary sum has reached next rule
          set l-rule-choice (l-counter - 1)                                    ;; set rule-choise
          set l-selection (l-selection + a-sus-window)                         ;; increase selection for the next inspection

         if debug [ debug-prnt (list "$0I0# l-selection=" (l-selection - a-sus-window) "  l-entity-choice=" l-entity-choice "  l-remainder=" l-remainder "  l-rule-choice=" l-rule-choice) ]
        ;;---------------------------------------- SUS end ---------------------------------------------
        ]
      ]
      set i-insp-entity l-entity-choice ;; entity to be inspected
      set i-insp-rule l-rule-choice     ;; rule to be inspected
    ]


    ;; ======================================= Inspection  =======================================
    move-to entity i-insp-entity

    ask entities-here [

      ;; Possible states for each entity-rule combination after the inspection are:
      ;;   1: not inspected and in compliance
      ;;  -1: not inspected and in violation
      ;;   2: inspected and in compliance (detected compliance)
      ;;  -2: inspected and in violation (detected violation)

      set l-rule-choice [i-insp-rule] of myself
      set l-asses-compl (item l-rule-choice e-lst-compliance)

;        if debug [ debug-prnt (list "$0I2# Noncompliance: Entity=" who ", rule=" l-rule-choice ) ]
      if (l-asses-compl = -1) [                      ;; if entity is non-compliant and
        if (random 101 > inspection-accuracy) [      ;; result larger than inspection-accuracy ==> inaccurate inspection
          set l-asses-compl 1                        ;; inspection incorrectly determines compliance
;          if debug [ debug-prnt (list "$0I3# Inaccurate inspection: Entity=" who ", rule=" l-rule-choice ) ]
        ]
      ]

      ifelse (l-asses-compl = 1) [                                      ;; inspection has determined compliance
        set e-lst-history replace-elem 0 l-rule-choice 2 e-lst-history
        set rp-trn-inspct-compli (rp-trn-inspct-compli + 1)
      ]
      [                                                                 ;; inspection has determined violation
        set e-lst-history replace-elem 0 l-rule-choice -2 e-lst-history
        set rp-trn-inspct-viol (rp-trn-inspct-viol + 1)
      ]

      if debug [ debug-prnt (list "$0J0# * Entity=" who "  e-lst-history="  e-lst-history ) ]

 ;     set l-t-lst (replace-item who l-t-lst (item 0 e-lst-history))
    ]

    set i-insp-result l-asses-compl

    ;; seting inspector's variables, depending on inspection's result
    ifelse (i-insp-result = 1) [           ;; compliance
      set color green
      set l-lst-ent replace-elem i-insp-entity i-insp-rule 1 l-lst-ent
    ]
    [                                      ;; violation
      set color red
      set l-lst-ent replace-elem i-insp-entity i-insp-rule -1 l-lst-ent
    ]

;    set g-i-history fput l-lst-ent g-i-history
    set rp-inspct-entities fput i-insp-entity rp-inspct-entities
    set rp-inspct-rules fput i-insp-rule rp-inspct-rules
  ]

;  set g-i-history fput l-t-lst g-i-history
  if debug [ debug-prnt (list "$0K0# <== play-agency") ]

end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;      SETUP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to setup

  let l-counter 0
  let l-lst-factor []

  ;; setup of debug properties - debug is written in "debug.txt" file in directory of .nlogo file
  if debug [
    carefully [ file-delete "debug.txt" ] []
    file-open "debug.txt"
    file-print "$000#---------------- ICARUS model - debug mode ----------------"
    file-print word "$000# Date & Time: " date-and-time
    file-close
  ]

  ;; setup of output file - output is written in .csv format in "output.csv" file in directory of .nlogo file
  if print-values [
    carefully [ file-delete "output.txt" ] []
    if (file-exists? "output.csv") [ file-delete "output.csv"]
    file-open "output.csv"
    file-print "tick;inspection strategy;entity;assessed inspection probability;punishment-size;e-lst-resources;e-lst-compliance"  ;; Header input
    file-close
  ]

  clear-all
  clear-output

  ask patches [ set pcolor grey ]

  ;; setting random seed
  ifelse (initial-seed = 0)
    [ random-seed new-seed ]      ; if initial seed is not set: set completely random seed
    [ random-seed initial-seed ]  ; otherwise, use specified seed

  ;; setting of global resorce requirements, taking into account input type
  ;; if resource requirements are read from the line, data should be in .csv format
  if ( (resource-requirements-type = "Input from line") or (resource-requirements-type = "Validation") ) [
    set g-i-lst-resources (csv:from-row resource-requirements)
    set number-of-rules (length g-i-lst-resources)
    if debug [ debug-prnt (list "$003# g-i-lst-resources=" g-i-lst-resources ", number-of-rules=" number-of-rules) ]
  ]

  if (resource-requirements-type = "Uniform distribution") [
    set g-i-lst-resources n-values number-of-rules
     [ precision (random-float resource-requirements-param ) 1] ]    ;; Normal

  if (resource-requirements-type = "Exponential distribution") [
    set g-i-lst-resources n-values number-of-rules
     [ precision (random-exponential resource-requirements-param ) 1] ]  ;; Otherwise, exponential

  ;; setting global learning parameters
  set g-memory-turns 100   ;; how many turns are remembered
  set g-lst-correction []  ;; corrrection factors

  repeat g-memory-turns [
    set g-lst-correction ( lput  ( 1 / (1 + l-counter * k-hyperbolic-discounting) ) g-lst-correction ) ;; calculating correction cefficient
    set l-counter (l-counter + 1)
  ]

  if debug [ debug-prnt (list "$006#  g-lst-correction=" g-lst-correction ) ]

  set g-i-history []

  ;; setting reporter values
  set rp-entity-violation n-values number-of-entities [0] ;; initialization of reporter
  set rp-rule-violation [-1]
  set rp-violations 0
  set rp-inspct-entities [-1]
  set rp-inspct-rules [-1]

  ;; create entites and inspection agency
  make-entities
  make-agency

  reset-ticks

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;      GO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to go
  if debug [ debug-prnt (list "$010# !!! ticks=" ticks) ]
  clean-up
  play-entities
  play-agency
  tick
end
@#$#@#$#@
GRAPHICS-WINDOW
357
69
1158
582
20
12
19.3
1
13
1
1
1
0
1
1
1
-20
20
-12
12
1
1
1
ticks
30.0

BUTTON
9
67
146
122
Setup
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
150
68
285
122
Go
Go
T
1
T
OBSERVER
NIL
G
NIL
NIL
0

SLIDER
25
288
302
321
number-of-entities
number-of-entities
0
200
194
1
1
NIL
HORIZONTAL

SLIDER
25
324
302
357
number-of-rules
number-of-rules
1
20
4
1
1
NIL
HORIZONTAL

SLIDER
27
471
301
504
resource-requirements-param
resource-requirements-param
1
5
1
0.1
1
NIL
HORIZONTAL

SLIDER
344
612
569
645
inspectors-capacity
inspectors-capacity
0
1
0.2
0.01
1
NIL
HORIZONTAL

TEXTBOX
14
129
170
154
Global setup:
16
0.0
1

TEXTBOX
315
16
1117
48
Multi-agent Model of Centrally Coordinated Compliance Inspection
26
0.0
1

SLIDER
26
547
304
580
max-deviation-resources
max-deviation-resources
0
0.5
0.31
0.01
1
NIL
HORIZONTAL

PLOT
1477
603
1776
766
Compl. & Viol. (TOTAL)
Tick
Violations
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"pen-1" 1.0 0 -2674135 true "" "if plot-all [ plot rp-violations ]"

CHOOSER
345
685
568
730
type-of-inspection-selection
type-of-inspection-selection
"Random" "Random entity" "Cycle" "Stohastic universal sampling"
1

PLOT
1477
430
1776
597
Inspected rules per entity
Entity
Inspected
0.0
10.0
0.0
10.0
true
false
"if plot-all [ clear-plot ]" "if plot-all [\nset-plot-pen-mode 1\nset-plot-x-range 0 number-of-entities\n]"
PENS
"default" 1.0 0 -955883 true "" "if plot-all [ histogram rp-inspct-entities ]"

PLOT
1473
71
1773
246
Inspected rules
Rule
Inspected
0.0
10.0
0.0
10.0
true
false
"if plot-all [\nclear-plot\nset-plot-pen-mode 1\nset-plot-x-range 0 number-of-rules\nplot-pen-reset ]" ""
PENS
"default" 1.0 1 -955883 true "" "if plot-all [ histogram rp-inspct-rules ]"

TEXTBOX
12
520
252
539
Entity-related variables:
16
0.0
1

TEXTBOX
328
585
576
604
Inspection-related variables:
16
0.0
1

SLIDER
344
733
568
766
rules-inspected-in-one-cycle
rules-inspected-in-one-cycle
1
20
4
1
1
NIL
HORIZONTAL

PLOT
1174
604
1470
766
Inspection res. (PER TICK)
Tick
Nr.
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"Comply" 1.0 0 -12087248 true "" "if plot-all [ plot rp-trn-inspct-compli ]"
"Violate" 1.0 0 -5298144 true "" "if plot-all [ plot rp-trn-inspct-viol ]"

PLOT
1171
432
1469
601
Compl. & Viol (PER TICK)
Tick
Nr.
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -13840069 true "" "if plot-all [ plot rp-trn-compliance ]"
"pen-1" 1.0 0 -2674135 true "" "if plot-all [ plot rp-trn-violations ]"

PLOT
1168
70
1468
244
Resource requirements
Rule
Resources
0.0
10.0
0.0
10.0
true
false
"if plot-all [\nclear-plot\nset-plot-pen-mode 1\nset-plot-x-range 0 number-of-rules\nset-plot-y-range 0 max g-i-lst-resources\nplot-pen-reset ]" ""
PENS
"default" 1.0 1 -6459832 true "if plot-all [ foreach g-i-lst-resources [plot ?] ]" ""

SLIDER
345
648
568
681
inspection-accuracy
inspection-accuracy
20
100
46
1
1
%
HORIZONTAL

SWITCH
24
159
158
192
debug
debug
1
1
-1000

MONITOR
893
598
1023
651
NIL
rp-violations
17
1
13

MONITOR
894
656
1024
709
NIL
rp-trn-violations
17
1
13

MONITOR
1029
713
1160
766
NIL
rp-trn-inspct-compli
17
1
13

MONITOR
895
712
1025
765
NIL
rp-trn-inspct-viol
17
1
13

SLIDER
27
619
306
652
max-risk-attitude-deviation
max-risk-attitude-deviation
0
100
94
1
1
%
HORIZONTAL

MONITOR
1027
598
1161
651
NIL
rp-compliance
17
1
13

MONITOR
1028
656
1160
709
NIL
rp-trn-compliance
17
1
13

SLIDER
27
655
305
688
k-hyperbolic-discounting
k-hyperbolic-discounting
0
5
0.2
0.1
1
NIL
HORIZONTAL

PLOT
1476
249
1777
428
Violations per entity (PER TICK)
Entity (who)
Violations
0.0
10.0
0.0
10.0
true
false
"if plot-all [\nclear-plot\nset-plot-pen-mode 1\nset-plot-x-range 0 number-of-entities\nset-plot-y-range 0 number-of-rules\nplot-pen-reset ]" ""
PENS
"default" 1.0 1 -16777216 true "" "if plot-all [\nplot-pen-reset\nforeach rp-entity-violation [plot ?] ]"

PLOT
1170
249
1471
428
Violations per rule (PER TICK)
Rule
Violations
0.0
10.0
0.0
10.0
true
false
"if plot-all [\nclear-plot\nset-plot-pen-mode 1\nset-plot-x-range 0 number-of-rules\nplot-pen-reset ]" ""
PENS
"default" 1.0 1 -16777216 true "" "if plot-all [ histogram rp-rule-violation ]"

INPUTBOX
167
222
302
282
punishment-size
208
1
0
Number

SLIDER
27
583
307
616
default-risk-attitude
default-risk-attitude
0
7
3
0.01
1
NIL
HORIZONTAL

CHOOSER
26
361
301
406
resource-requirements-type
resource-requirements-type
"Input from line" "Uniform distribution" "Exponential distribution" "Validation"
0

INPUTBOX
28
408
301
468
resource-requirements
1.5,6,5.5,15
1
0
String

INPUTBOX
167
159
302
219
initial-seed
0
1
0
Number

SWITCH
29
690
304
723
stackelberg-aware
stackelberg-aware
1
1
-1000

BUTTON
9
11
145
62
profile
profile
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
24
196
158
229
plot-all
plot-all
0
1
-1000

SWITCH
24
234
158
267
print-values
print-values
1
1
-1000

CHOOSER
29
726
201
771
learning-mechanism
learning-mechanism
"Fictitious play" "Reinforcement learning"
0

BUTTON
153
11
285
62
NIL
validate
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
206
725
304
758
risk-exp
risk-exp
1
1
-1000

@#$#@#$#@
## WHAT IS IT?

A multi-agent compliance inspection model (ICARUS - _Inspecting Compliance to mAny RUleS_). The model is applicable to environments where an inspection agency, via centrally coordinated inspections, examines compliance of organizations which must comply with multiple provisions.
The model should allow verification of whether inspection strategies that rely on the assumption of different frequency of violations of different provisions are more effective than the often used inspection strategies (Cyclic, random).

## HOW IT WORKS

The model (ICARUS) contains 3 types of agents:

  1. entities,
  2. inspection agency and
  3. inspectors / inspections

ICARUS describes a repeated, simultaneous, non-cooperative game of pure competition. Agents have imperfect, incomplete, asymmetric information. Entities in each move (tick) choose a pure strategy (comply/violate) for each rule, depending on their own subjective assessment of the probability of the inspection. The Inspection Agency carries out the given inspection strategy.


### Entities

Each entity is obliged to comply with all the established social norms (rules or regulations). Each entity in each discrete time interval (tick) decides whether to comply with or violate each rule. When an entity decides whether to comply with or violate each rule, it does not know whether it will be subjected to an inspection.

Entities' decision mechanisms are based on the theory of rational choice and the game theory. The strategies of the inspection agency are based on simplified patterns identified in empirical research.

Entities are characterized by uniqueness, heterogeneity, explicit goals, autonomy, locality, flexibility and limited rationality. In addition, entities have the following characteristics:

  * Entities are rational, but their rationality is bounded because of the limited scope of the data on which decisions are made.
  * Entities are heterogeneous, given their willingness to take risks and the resources required for compliance.
  * Entities learn in line with the fictitious play model and make decisions about the future.
  * Entities have a present bias, i.e. they attach greater value to recent experiences.
  * Entities do not communicate with each other.

Each entity is characterized by a resource requirements vector, i.e. the resources required to fulfill each rule. Entities also differ in their willingness to take risks, i.e. "risk appetite". Entities record the history of their actions and the inspections they were exposed to. At any moment, each entity knows the state of its compliance with all the rules and the results of all the inspections in the recorded history.

Entities make boundedly rational decisions on whether to comply with or violate each rule, comparing the cost of compliance and the expected value of punishment. Entities estimate the probability of the inspection based on their risk appetite, known inspection history, and the temporal discounting index, or the measure of sensitivity to delay, modeled via hyperbolic function.


### The Inspection Agency

The Inspection Agency monitors compliance of entities with all rules. In each discrete time interval (tick), the Agency decides whether to inspect each of the possible combinations (pairs) of entities and rules. The Agency does not know the state of compliance of entities with the rules when it decides which combination of entities and rules will be include in inspections at a given time interval. The Agency monitors compliance via inspections.

The Inspection Agency makes assumptions about the resources required to comply with each provision. The Agency determines which entities and rules will be monitored at each time interval (tick), depending on the selected inspection strategy and the inspection capacity. The inspection capacity determines how many entity-rule combinations can be inspected at any time interval.

The Inspection Agency may apply several inspection strategies:

  * By applying the **Random** strategy, the agency randomly selects entities and rules for inspekction. Each entity-rule combination has the same probability of inspection.
  * By applying the **Random entity** strategy, the agency randomly selects the entities that will be inspected. All the rules in the selected entities are checked for compliance.
  * By applying the **Cyclic** strategy, inspections sequentially inspect entities, until all are covered, which indicates completion of the inspection cycle. After that, a new cycle begins. One or more rules may be included in one cycle.
  * By applying the **Stohastic universal selection (SUS)** strategy, the inspection agency randomly selects entities and provisions to be inspected. The probability of inspection of individual provisions varies, depending on the assumptions about the resources required to comply. That is, the rule that the inspection agency believes to require greater resources for achieving / maintaining compliance will be proportionately more often covered by the inspection.


### Inspections

Inspections are agents that can verify the compliance of any entity with one or more rules. Inspections are directed by the Inspection Agency. At any moment, one inspection can check the compliance of only one entity. The inspection determines whether the entity complied with the supervised rules at the time of inspection. The inspection then informs the agency of inspection result.


Spatial postions of entities has no impact on model performance and simulation results. However, interaction between entities and inspections are visually displayed by moving avatars. These moves are conditioned by the selected inspection strategy and the spatial location of entites. The influence of time on the behavior of the inspection agency depends on the chosen inspection strategy. Time does not affect a random inspection strategies and the Stohastic universal selection, but it affects cyclic strategies. Namely, the choice of entities and the rules to be covered in further steps in cyclic strategies depends on the previous steps of the inspection cycle.



## HOW TO USE IT

### Interface

The interface is divided into 3 parts:

  1. left: input parameters,
  2. central: graphical display of the results of the current simulation step,
  3. right: quantitative and descriptive statistics of the current simulation.

The central part of the interface graphically displays the current simulation step. Entities, the Inspection Agency and inspections are shown with different icons. At the beginning of the simulation (`setup()`), the given number of entities (`number-of-entites`) is randomly placed, each entity having a unique numeric mark. In each step of the simulation (`go()`), the inspections' icons are positioned on the selected entity and the inspection is performed. The color of the inspection (inspector) indicates whether the inspection in question has revealed compliance (green) or violation (red). The entity's color indicates how many rules it is violating (darker red indicates that the entity violates more rules). The current step (`tick`) is visible.


### Input variables

#### Simulation runtime

  * `debug`: print debug data?
  * `plot-all`: draw the results of the simulation on the user interface diagrams?
  * `print-values`: results of the simulation are printed in the output file?
  * `initial-seed`: allows seed input to be used to generate random values. If the parameter value is 0, the seed will be generated randomly at the beginning of the simulation. With identical (predefined) initial seed, all simulation steps will have the same results.

#### Environment

The `resource-requirements`, `resource-requirements-type`, and `resource-requirements-param` parameters are connected to the knowledge of the Inspection Agency, about the resource requirements required to fulfill each provision. The `resource-requirements-type` parameter allows the user to select one of the following options: _Input from line_, _Uniform distribution_, _Exponential distribution_, and _Validation_.

  * If the _Input from line_ option is selected, the user needs to manually enter the resource requirements required to meet each provision manually in the `resource-requirements` parameter.
  * If a user chooses _Uniform distribution_ then the resource requirements for fulfilling each provision will be U[0, `resource-requirements-param`].
  * If a user chooses _Exponential distribution_, then the resource requirements for the fulfillment of each provision will be determined as the value of the random variable with the exponential distribution whose arithmetic mean is the value of the `resource-requirements-param`.
  * By selecting the _Validation_ option, resource requirements are defined through validation procedure `validate()`.


The `default-risk-attitude`, `max-risk-attitude-deviation` and `risk-exp` parameters are related to the setting risk appetite preferences. If the `risk-exp` variable is activated (`TRUE`) then each entities' risk appetite is the value of the random variable with the exponential distribution whose arithmetic mean is the `default-risk-attitude` parameter. If the `risk-exp` variable is not activated (`FALSE`), then each entities' risk appetite will be determined as a random variable with a uniform distribution in the range whose mean is the value of the `default-risk-attitude` variable and the upper and lower limits are the percentage deviation from these values, set by the `max-risk-attitude-deviation` variable. The risk appetite of each entity is constructed dynamically at the beginning of the simulation in the `make-entity()` procedure. Risk appetite of 1 means that the entity is perfectly rational in making decisions. Risk appetite greater than 1 means that the entity is risk-taker, and risk appetite less than 1 means that the entity is risk-averse.

#### Entities

Overview of the entities-related parameters:

  * `learning-mechanism` allows users to choose one of 2 possible learning methods: _Fictitious play_ and _Reinforcement learning_. All entities in the model learn using the selected method.
  * If the variable `stackelberg-aware` is activated (`TRUE`), all entities know that the inspection agency applies the _SUS_ inspection strategy and estimate the probability of inspection of each provision, taking into account their own compliance resource requirements.
  * The `max-deviation-resource` parameter defines the largest possible deviation in the resource requirements for compliance with each individual rule from the resource requirements expected by the Inspection Agency.
  * The impact of past experience is weakened over time (hyperbolic discounting), with the reduction rate dependent on the parameter `k-hyperbolic-discounting`.



#### The Inspection Agency and inspections

Overview of the parameters related to the Inspection Agency and inspections:

  * `inspectors-capacity` defines the ratio of entity-rule combinations that can be inspected in one `tick`.
  * `inspection-accuracy` determines how accurately inspections determine violations (only type I error - false positive).
  * The `type-of-inspection-selection` allows the user to select one of the 4 possible methods of selecting the inspection pattern: _Random, Random Entity, Cycle_ and _Stohastic universal sampling_. The Inspection Agency uses the selected method throughout the simulation to direct the inspections of entities. The Inspection Agency is characterized by several variables. The subset of variables that will be used depends on the applied inspection strategy, i.e. the value of the variable `type-of-inspection-selection`.



### Reporting variables and diagrams

Numeric indicators:

  1. `rp-violations`: total (accumulated) number of violation during the simulation; includes violations observed by inspections, violations that the inspections did not recognize as well as violations that were not subject to inspections.
  2. `rp-compliance`: total (accumulated) number of rules compliant with regulations; includes the compliance observed by inspections, as well as compliance not covered by  inspections.
  3. `rp-trn-violations`: The number of rules' violations in the current simulation step (observed or not by inspection).
  4. `rp-trn-compliance`: The number of compliant rules in the current simulation step (observed or not by inspection).
  5. `rp-trn-inspect-viol`: the number of violations that were observed by the inspections performed in the current simulation step.
  6. `rp-trn-inspct-comp`: the number of compliant rules that were observed by the inspections performed in the current simulation step.

The right side of the graphical interface displays the results of the simulation, some of which relate to the current simulation step (the "Per TICK" tag in the diagram name), while others display accumulated simulation results.

## THINGS TO NOTICE

The key measure is the total number of violations (`rp-violations`). Observe how changes in input parameters influence the total number of violations.


## CREDITS AND REFERENCES

https://github.com/ssmojver/DS

Built under NetLogo 5.3.1 (download: https://ccl.northwestern.edu/netlogo/5.3.1/)
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
Polygon -13840069 true false 238 112 252 141 219 141 218 112
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
NetLogo 5.2.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="H-test-IT-03" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>ticks = 25</exitCondition>
    <metric>rp-violations</metric>
    <enumeratedValueSet variable="number-of-entities">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-rules">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-requirements-param">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inspectors-capacity">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-deviation-resources">
      <value value="0.31"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="type-of-inspection-selection">
      <value value="&quot;Random&quot;"/>
      <value value="&quot;Random entity&quot;"/>
      <value value="&quot;Cycle&quot;"/>
      <value value="&quot;Stohastic universal sampling&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rules-inspected-in-one-cycle">
      <value value="2"/>
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inspection-accuracy">
      <value value="46"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="debug">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-risk-attitude-deviation">
      <value value="94"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k-hyperbolic-discounting">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="punishment-size">
      <value value="208"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="default-risk-attitude">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-requirements-type">
      <value value="&quot;Input from line&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-requirements">
      <value value="&quot;1.5,6,5.5,15&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-seed">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stackelberg-aware">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plot-all">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="print-values">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning-mechanism">
      <value value="&quot;Fictitious play&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="risk-exp">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="H-test-DK-03" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>ticks = 25</exitCondition>
    <metric>rp-violations</metric>
    <enumeratedValueSet variable="number-of-entities">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-rules">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-requirements-param">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inspectors-capacity">
      <value value="0.33"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-deviation-resources">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="type-of-inspection-selection">
      <value value="&quot;Random&quot;"/>
      <value value="&quot;Random entity&quot;"/>
      <value value="&quot;Cycle&quot;"/>
      <value value="&quot;Stohastic universal sampling&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rules-inspected-in-one-cycle">
      <value value="2"/>
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inspection-accuracy">
      <value value="57"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="debug">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-risk-attitude-deviation">
      <value value="82"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k-hyperbolic-discounting">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="punishment-size">
      <value value="136"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="default-risk-attitude">
      <value value="1.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-requirements-type">
      <value value="&quot;Input from line&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-requirements">
      <value value="&quot;23,12,14,5,8,11&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-seed">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stackelberg-aware">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plot-all">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="print-values">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning-mechanism">
      <value value="&quot;Fictitious play&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="risk-exp">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="H-test-US-03" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>ticks = 25</exitCondition>
    <metric>rp-violations</metric>
    <enumeratedValueSet variable="number-of-entities">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-rules">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-requirements-param">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inspectors-capacity">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-deviation-resources">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="type-of-inspection-selection">
      <value value="&quot;Random&quot;"/>
      <value value="&quot;Random entity&quot;"/>
      <value value="&quot;Cycle&quot;"/>
      <value value="&quot;Stohastic universal sampling&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rules-inspected-in-one-cycle">
      <value value="2"/>
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inspection-accuracy">
      <value value="97"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="debug">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-risk-attitude-deviation">
      <value value="96"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k-hyperbolic-discounting">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="punishment-size">
      <value value="166"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="default-risk-attitude">
      <value value="4.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-requirements-type">
      <value value="&quot;Input from line&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-requirements">
      <value value="&quot;1.8,1.4,1.8,1.1,1.7,1.9,9.6,9.3,9.7,9.6&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-seed">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stackelberg-aware">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plot-all">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="print-values">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning-mechanism">
      <value value="&quot;Fictitious play&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="risk-exp">
      <value value="false"/>
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
