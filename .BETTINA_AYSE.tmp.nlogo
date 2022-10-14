;__________________________________________________________________________________________________________________________________________
;__________________________________________________________________________________________________________________________________________
;______________________________________________________________BETTINA-AYSE-IBM____________________________________________________________
;__________________________________________________________________________________________________________________________________________
;__________________________________________________________________________________________________________________________________________

;__________________________________________________________________________________________________________________________________________
;________________________________________________________________Extensions________________________________________________________________
;__________________________________________________________________________________________________________________________________________
extensions [
  csv
  fetch
]

;__________________________________________________________________________________________________________________________________________
;__________________________________________________________________Breeds__________________________________________________________________
;__________________________________________________________________________________________________________________________________________
breed [roots root]
breed [trees tree]

;__________________________________________________________________________________________________________________________________________
;___________________________________________________Globals, Patches and Turtles Variables_________________________________________________
;__________________________________________________________________________________________________________________________________________
globals [
  ; Time related variables
  Time                ; Not really time, but variable that specifies at which time point of the simulations we are.
  Year                ; Current model year                   [1 - inf]
  Month               ; Current model month                  [1 - 12]
  Day                 ; Current model day                    [1 - 31]
  Hour                ; Current model hour                   [0 - 23]
  JulianDay           ; Julian day of the year               [1 - 366]
  nDays               ; Number of days in the current year   [365 / 366]
  POSIXct             ; Current model time                   [YYYY-MM-DD HH:MM:SS]

  ; Climate variables
  ClimateData         ; Input climate data from DWD. Is important from csv to NetLogo list of lists  []
  nRow                ; Current model data row [1 - xyz]

  Precipitation       ; Current model precipitation (per time step) [mm]
  E_Precipitation     ; Precipitation during a precipitation event. [mm] ; Precipitation event = followed by 4 hours without precipitation (Staelense et al. 2008, Xiao et al. 2000)
  h_E_Precipitation   ; Amount of hours without precipitation. Used to calculate the amount of rainfall per precipitation event.
  I1
  I2
  S1
  S2

  ; Ground water
  depth               ; Depth of roots                                             [m]
  v_pore              ; Proportion of pore volume per soil volume                  [m³ / m³]
  v_water_max         ; Proportion of maximum water volume per soil volume         [m³ / m³]
  v_water_min         ; Proportion of water volume that trees won't be able to get [m³ / m³] This is used to cap the calculation of the reverse pF-curve cause numbers are too large otherwise

  ; These are growth variables describing the growth allocation
  SigmoidSlopeHeight  ; Maximum slope of the sigmoid function that describes ressources allocation (Peters et al. 2014, Eq. 27), Height growth
  SigmoidSlopeRadius  ; Maximum slope of the sigmoid function that describes ressources allocation (Peters et al. 2014, Eq. 27); Radial growth

  ; General Tree Parameters
  Species-Parameter   ; List of Parameter-Values for each species

  ; Basic variables for various parts of the model
  size-factor         ; Size regulation (for visuals), change the size of the turtles in the UI
  Min_Size            ; Minimum size that turtles can have to still be visible
  Gravity             ; Gravitational acceleration (approx. 9.81) [m / s²]

  ; Recording Data
  t_precipitation     ; total precipitation                 [m³ / m²]
  t_infiltration      ; total infiltration                  [m³ / m²]
  t_interception      ; total interception                  [m³ / m²]
  t_stemflow          ; total stemflow                      [m³ / m²]
  t_gravity           ; total gravitational water           [m³ / m²]
  t_uptake            ; total water uptake of trees         [m³ / m²]

  Overstory-Data      ; Growth Data from Overstory-Trees for export  [list of lists]
  Regeneration-Data   ; Growth Data from Regeneration for export     [list of lists]
  Patches-Data        ; Patches Data for export                      [list of lists]
]


patches-own [
  r_water         ; Water ratio - percentage of water volume per ground volume                [m³ / m³]
  c_water         ; Water content - total amount of water per ground area                     [m³ / m²]
  l_interception  ; interception loss caused by trees                                         [%]
  c_infiltration  ; Infiltration content - total amount of water infiltrating into the ground [m³ / m]
  p_infiltration  ; Total patch Infiltration                                                  [m³]
  psi_matrix      ; Matrix potential (power needed to get the water out of soil               [Pa]
  v_water_gravity ; Amount of gravitational water                                             [m³]
  p_light         ; Proportion of Light Reaching the Ground (or a Tree)                       [0 - 1]
  Zone            ; 3 Zones are defined in the experimental design (Core, Border, Edge)?      [factorial]
]


trees-own [
  Species       ; Tree Species        [factorial]
  Layer         ; Are Trees from overstory or regeneration?

  ; These are all the size measures of the trees:
  r_stem        ; Stem Radius         [m]
  r_crown       ; Crown Radius        [m]
  r_root        ; Root Radius         [m]
  h_stem        ; Stem Height         [m]
  h_crown       ; Crown Height        [m]
  h_root        ; Root Height (Depth) [m]
  v_stem        ; Stem Volume         [m³]
  v_crown       ; Crown Volume        [m³]
  v_root        ; Root Volume         [m³]

  v_leaf        ; Leaf Volume         [m³]
  v_branch      ; Branch Volume       [m³]
  v_cableroot   ; Cable Root Volume   [m³]
  v_fineroot    ; Fine Root Volume    [m³]
  v_tree        ; Tree Volume         [m³]
  growth        ; Growth of the Tree  [m³] per tick

  g_h_stem      ; Stem Height Growth  [m]
  g_r_stem      ; Stem Radius Growth  [m]
  g_r_crown     ; Crown Radius Growth [m]
  g_r_root      ; Root Radius Grwoth  [m]

  ; These are the hydraulic attributes
  L_p             ; Hydraulic Permeability of Root Skin           [(m / (s * Pa)]
  psi_leaf        ; Leaf Water Potential                          [Pa]
  tree_psi_matrix ; Mean Matrix Potential of surrounding patches [Pa]
  k_f_sap         ; Hydraulic Conductivity of sap flow path       [m² / (s * Pa)]
  r1              ; Root Surface Resistance. Resistance for water to go through the root skin  [(s * Pa) / m³]
  r2              ; Sap Flow Resistance. Resistance for water to go through the stem           [(s * Pa) / m³]
  v_water         ; Amount of water the tree uses per tick        [m³]

  ; Variables for Shading
  t_light       ; Proportion of light that is absorbed by tree crown [0 - 1]

  ; Variables concerning resource availabilities and resource allocation
  RES_a         ; Growth restriction of above ground resource availability [m³ / s]
  RES_b         ; Growth restriction of below ground resource availability [m³ / s]
  RES_avail     ; Overall restriction (minimum of RES_a and RES_b)         [m³ / s]
  RES_sum       ; Summed up water usage                                    [m³]

  w_h_stem      ; Proportion of growth that is attributed to stem  height growth   [0 - 1]
  w_r_crown     ; Proportion of growth that is attributed to crown radius growth   [0 - 1]
  w_r_stem      ; Proportion of growth that is attributed to stem  radius growth   [0 - 1]
  w_r_root      ; Proportion of growth that is attributed to root  radius growth   [0 - 1]

  ; These are other parameters
  Photo?        ; Parameter defining if a Tree is doing Photosynthesis or not (depeding on Daytime and Season) [logical]
  Leaf?         ; Parameter defining if a Tree is able to do Photosynthesis depending if Leafs are already out [logical]
  h_max         ; maximum proportion of total growth that is allocated to height growth [(0 - 1)]
  k_grow        ; Scaling of growth per time step [-]
  k_maint       ; maintainance cost [1 / s]
  solar_r       ; solar irradiation [m / s]
  rootID        ; ID of the corresponding root.

  ; Variables Ronny told me not to touch
  k_geom        ; ALTER, I'm not sure what the equivalent is, it scales L_p (hydraulic conductivity of root skin) to below-ground growth restriction,
                ;                                             if bigger, the below-ground growth restriction gets bigger (is not weighted as much)
  k_rel         ; Weighting of the growth restrictions, if = 1, restriction above and restriction below are equally weighted,
                ;                                       if > 1, restriction above is more important then restriction below     [-]
]


;__________________________________________________________________________________________________________________________________________
;____________________________________________________________Start of Procedures___________________________________________________________
;__________________________________________________________________________________________________________________________________________

;----------------------------------------------------------------------
;--- Setup of Simulation
;----------------------------------------------------------------------
to setup
  ca                  ; Reset all parameters to [-].
  reset-ticks         ; Reset the model time.
  import-climate      ; Import Climate Data.
  initialize-world    ; Create World with Initial Parameters.
  initialize-trees    ; Create Trees.
end


to go
  tick
  update-time
  update-climate
  shade
  ask trees [
    if Photo? = TRUE [
      Potential-Growth
      Water-Uptake
      Allocation
      Grow
    ]
  ]

end


;__________________________________________________________________________________________________________________________________________
;______________________________________________________________Setup Procedures____________________________________________________________
;__________________________________________________________________________________________________________________________________________

;----------------------------------------------------------------------
;--- Import Climate Data
;----------------------------------------------------------------------
to import-climate
  ; set ClimateData csv:from-file "Climate_Data.csv"  ; Load Climate from file (found in ZIP-Data)
  set ClimateData data.frame (read-from-string (word "[" fetch:url "https://raw.githubusercontent.com/JHeinermann/VERMOS_open/main/Climate_Data.txt" "]")) 9
end

;----------------------------------------------------------------------
;--- Set all important global parameters
;----------------------------------------------------------------------
to initialize-world
  ; Set time related variables
  set Time 1                          ; Set the Time to 1 (Time defines the model time. if the time value exceeds the length of Climate Data, Time is reset to 1.
  set Year item 2 item 1 ClimateData  ; Set the Year depending on the first year of Climate Data.
  set nRow item Time ClimateData
  let TimeFormat item 5 nRow                                     ; ... and Displayed Time (POSIXct-format).
  if TimeFormat < 10 [set TimeFormat (word 0 TimeFormat)]
  set POSIXct (word item 1 nRow " " TimeFormat ":00:00")

  ; Set ressource allocation parameters
  set SigmoidSlopeHeight 0.3          ; Set the slope of a sigmoid function concerning ressource allocation (see Peters et al. 2014, Eq. 27).
  set SigmoidSlopeRadius 0.015         ; Set the slope of a sigmoid function concerning ressource allocation (see Peters et al. 2014, Eq. 27).


  ; Basic model parameters
  set size-factor 1                 ; Set the size of the turtles in the UI; this is only for visuals.
  set Min_Size 3 / patch-size         ; In order to be able to see even the smallest trees, tree size in the UI is set to a minimum.

  ; Set Tree variables
  set-default-shape trees "circle3"  ; All Trees are circles.
  set-default-shape roots "circle"  ; All Trees are circles.
  set Species-Parameter (list         ; Set Values for Species-specific variables:
    (list "Species"                   ; Species in Characters.
      "Pinus sylvestris" "Quercus petraea" "Fagus sylvatica" "Betula pendula" "Sorbus aucuparia" "Quercus rubra" "Pinus strobus")
    (list "k_f_sap"                   ; Hydraulic Conductivity of sap flow path [m² / (s * Pa)].
      4.170e-10 2.980e-9 1.641e-9)
    (list "psi_leaf"                  ; Leaf Water Potential                    [Pa].
      -3201000 -5000000 -3477000) ; For Pine it was -3201000 before
    (list "L_p"                       ; Hydraulic Permeability of Root Skin     [(m / (s * Pa)].
      (7.600e-15 * 0.55) 0.491e-14 1.700e-14)
    (list "h_crown"                   ; Crown Height                            [m]
      (0.00058 * 2) (0.00088 * 2) (0.00106 * 2))
    (list "h_root"                    ; Root Height                             [m]
      0.001355 0.00123 0.00104)  ; For pine it was 0.00166 before
    (list "k_grow"                    ; Growth Speed (Scaling of Growth per time Step [-]
      0.018 (0.0034 * 1.5) 0.0042)
    (list "h_max"                     ; maximum proportion of total growth that is allocated to height growth [(0 - 1)]
      0.35 0.019 0.062)
    (list "color"                     ; RGB-value of tree colors.
      (list 204 154 28) (list 252 242 76) (list 156 242 4) (list 252 78 252) (list 188 190 252) (list 252 242 76) (list 204 154 28))
  )

  ; Below-ground settings
  set depth 1                                                         ; Water availabilities for the first meter below surface are caltulated [m]
  set v_pore (pF w_s w_r alpha n 0) * depth                           ; The total pore volume is calculated using [m³]
  set v_water_max (pF w_s w_r alpha n (10 ^ FK)) * depth              ; The maximum amount of water in 1 m³ soil is calculated on the basis of Field Capacity (soil moisture held against gravity) [m³]
  set v_water_min (pF w_s w_r alpha n (-1 * (min (list 0 -7860000)))) ; To avoid calculation of very large numbers within the reverse-pF-function, a minimum soil water content is calculated [m³]

  ; Categorize patches into 3 categories: "Edge" (Outer patches), "Border" (inner 20 x 20 patches), "Core" (inner 10 x 10 patches)
  ask patches [                    ; Ask all patches...
    set c_water v_water_max        ; ... set the current soil water content to the maximum...
    set Zone "Edge"                ; ... and the Zonation to "Edge".
  ]
  ask patches with [abs pxcor <= 10 AND abs pycor <= 10] [  ; For the inner 20 x 20 patches...
    set Zone "Border"                                       ; ... set the zonation to "Border"...
    if abs pxcor <= 5 AND abs pycor <= 5[                   ; ... and for the inner 10 x 10 patches...
      set Zone "Core"                                       ; ... set the zone to "Core".
    ]
  ]

  ifelse is.divisible Year 4 [
    set nDays 366
  ] [
    set nDays 365
  ]

  Color-Patches                 ; Finally, color patches depending on their zonation and most important: on their water content or soil suction.
  Plot-Water-Retention-Curve    ; And also plot the Water Retention Curve.
end

;----------------------------------------------------------------------
;--- Set patch color depending on soil water content / soil suction (and zonation)
;----------------------------------------------------------------------
to Color-Patches
  ; A lineare function is calculated. The color of a patches is linearly dependent on soil water content or soil water content.
  let ValueMax v_water_max                                        ; Save the maximum soil water content...
  let ValueMin v_water_min                                        ; ... and the minimum soil water content...
  if Patches-Color = "Soil Suction" [                             ; ... or, if patches are chosen to be colored depending on patches soil suction...
    set ValueMax 100 * (reverse-pF w_s w_r alpha n v_water_max)   ; ... override the maximum value with the respective soil suction pressure...
    set ValueMin 100 * (reverse-pF w_s w_r alpha n v_water_min)   ; ... and override the minimum value with the respective minimal soul suction pressure.
  ]
  let colorB ((92 - 98) / (ValueMax - ValueMin))                  ; After calculating maximum and minimum values of the linear function...
  let colorA (98 - (ValueMin * colorB))                           ; ... calculate slope (ColorB) and intercept (ColorA) of this linear function.
  ask patches [                                                   ; Color patches depending on their water content (Vol-%).
    let ValueX c_water                                            ; Set the Function input to Soil Water Content...
    if Patches-Color = "Soil Suction" [                           ; ... and change it, if one chooses to color patches depending on Soil Suction Pressure...
      set ValueX psi_matrix                                       ; ... to said Soil Suction Pressure.
    ]
    set pcolor colorA + colorB * ValueX                           ; Now calculate patch color from the previously formed linear function.
    if Zone = "Edge" [                                            ; Patches within the "Edge" or "Core"-Zone will get a slightly different color.
      set pcolor pcolor + 10
    ]
    if Zone = "Core" [
      set pcolor pcolor - 10
    ]
  ]
end

;----------------------------------------------------------------------
;--- Plot the Water Retention Curve in the interface
;----------------------------------------------------------------------
to Plot-Water-Retention-Curve
  set-current-plot "Water-Retention-Curve"                        ; Set the current plot to the desired "Water-Retention-Curve"-Plot.
  let plotx v_water_min                                           ; NetLogo is plotting straight lines by connecting a new point to a previous point. Define the first point here.
  let ploty (log (reverse-pF w_s w_r alpha n v_water_min) 10)     ; x is the minimum water content and y the respective pF-value.
  while [plotx < w_s] [                                           ; While x is smaller than the maximum possible water content (not taking field capacity into account)...
    set ploty (log (reverse-pF w_s w_r alpha n plotx) 10)         ; ... calculate the new y-value...
    plotxy (plotx * 100) ploty                                    ; ... plot x and y...
    set plotx plotx + 0.001                                       ; ... and then increase x just a little bit.
  ]
  if matric-potential = "fixed" [                                 ; In case the soil suction pressure is fixed during the simulation, plot a horizontal and a vertical blue line at the fixed soil suction pressure.
    plot-pen-up
    set-plot-pen-color blue
    plotxy 0 fixed-pF
    plot-pen-down
    plotxy 100 fixed-pF
    plot-pen-up
    plotxy ((pF w_s w_r alpha n (10 ^ fixed-pF)) * 100) 0
    plot-pen-down
    plotxy ((pF w_s w_r alpha n (10 ^ fixed-pF)) * 100) 7
  ]
end

;----------------------------------------------------------------------
;--- Create all trees and set their initial values
;----------------------------------------------------------------------
to initialize-trees
  if Overstory? [                                                ; Create Overstory...
;    let Overstory_Data csv:from-file "Overstory_Data.csv"        ; ... from external Overstory data (must be stored at same location as model).
    let Overstory_Data data.frame (read-from-string (word "[" fetch:url "https://raw.githubusercontent.com/JHeinermann/VERMOS_open/main/Overstory_Random.txt" "]")) 11
    foreach Overstory_Data [                                     ; For each line of data (each tree)...
      x -> create-trees 1 [                                    ; ... create one tree...
        initialize-tree-parameters (item 3 x) (item 4 x) (item 5 x) (item 6 x) (item 7 x) (item 8 x) (item 9 x) (item 10 x) "Overstory"      ; ... and initialize it with the data found in the external data.
      ]
    ]
  ]

  if Regeneration? [                                             ; Create Regeneration...
    ;let Regeneration_Data csv:from-file "Regeneration_Data.csv"  ; ... from external Regeneration data (must be stored at same location as model).
    let Regeneration_Data data.frame (read-from-string (word "[" fetch:url "https://raw.githubusercontent.com/JHeinermann/VERMOS_open/main/Regeneration_Random.txt" "]")) 11
    foreach Regeneration_Data [                                  ; For each line of data (each tree)...
      x -> create-trees 1 [                                    ; ... create one tree...
        initialize-tree-parameters (item 3 x) (item 4 x) (item 5 x) (item 6 x) (item 7 x) (item 8 x) (item 9 x) (item 10 x) "Regeneration"   ; ... and initialize it with the data found in external data
      ]
    ]
  ]
end

;----------------------------------------------------------------------
;--- Set tree parameters based on input from external csv-Data
;----------------------------------------------------------------------
to initialize-tree-parameters [.Species .Species_ID .xcor .ycor .r_stem .h_stem .r_crown .r_root .Layer]   ; (.Species is used because there is a Tree-Variable called "Species" -> .Species is used in this procedure only)
  let Species_Index .Species_ID     ; Tree variables are initialized based on species specific values. At the moment only 3 species (Pinus sylvestris, Quercus petraea, Fagus sylvatica) are included in the model...
  if .Species_ID > 3 [              ; ... all other species (ID > 3)...
    set Species_Index 1             ; ... are treated as Pinus sylvestris...
    if Species_Index = 6 [          ; ... except for Quercus rubra...
      set Species_Index 2           ; ... which is treated as Quercus petraea.
    ]
  ]

  ; Now set the species and initial (allometric) values from external data
  set Species .Species    ; Species      [Pine, Oak, Beech]
  set xcor    .xcor       ; X-Position   [m]
  set ycor    .ycor       ; Y-Position   [m]
  set r_stem  .r_stem     ; Stem Radius  [m]
  set h_stem  .h_stem     ; Stem Height  [m]
  set r_crown .r_crown    ; Crown Radius [m]
  set r_root  .r_root     ; Root Radius  [m]
  set Layer   .Layer      ; Layer        [Overstory / Regeneration]


  ; Species specific variable values are stored in a list in NetLogo. Get them from this list (see "Species-Parameter" in "initialize-world")
  set color    (item .Species_ID (item 8 Species-Parameter))    ; Color in the overworld (Colors according to ForstBrandenburg, MLUV 2006)
  set h_crown  (item Species_Index (item 4 Species-Parameter))  ; Crown "Height"                            [m]
  set h_root   (item Species_Index (item 5 Species-Parameter))  ; Root "Height"                             [m]
  set L_p      (item Species_Index (item 3 Species-Parameter))  ; Hydraulic Permeability of Root Skin       [(m / (s * Pa)]
  set psi_leaf (item Species_Index (item 2 Species-Parameter))  ; Leaf Water Potential                      [Pa]
  set k_f_sap  (item Species_Index (item 1 Species-Parameter))  ; Hydraulic Conductivity of sap flow path   [m² / (s * Pa)]
  set k_grow   (item Species_Index (item 6 Species-Parameter))  ; Scaling of growth per time step           [-]
  set h_max    (item Species_Index (item 7 Species-Parameter))  ; maximum proportion of total growth that is allocated to height growth [(0 - 1)]
  set k_maint 0                                                 ; Maintainance costs                        [m³ / m³]

  ;____________________________________________________________________________________________________________________________________
  ;___________________________________________________General - INITIALIZATION_________________________________________________________
  ;____________________________________________________________________________________________________________________________________

  ; Set Growth related parameter
  set k_rel 1               ; set the weighting of the restriction above to restriction below
  set k_geom 4000           ; set the Root skin surface per m³ ground [m² / m³]
  if .Species_ID = 1 [
    set k_geom 4000
  ]

  ; Set light (/shadowing) related parameters
  set solar_r 2.5e-8        ; Solar irridiation [m / s]
  set t_light 0.05          ; Crown transparency for light (0 - 1; 1 = Transparent)

  ; Set parameters of Interception
  set I1 0.941
  set I2 -0.604
  ; Set parameters of Stemflow
  set S1 -0.209
  set S2 0.098

  ; Set Size
  set size (r_crown * size-factor * 2)   ; Set the size (only for visualisation in the UI)...
  if size < Min_Size [                   ; ... if the Size of Trees is too small, Trees are not displayed in the Overworld, so if the Size is smaller than the smallest displayed Size...
    set size Min_Size                    ; ... Set the Size of Trees to the minimum Size.
  ]
  __set-line-thickness 0.3

  ; If the roots should be plotted also:
  if Plot-Roots? [
    let r_root_t r_root                      ; Create a local variable storing information of the Root Radius of Trees...
    let RootNr 0                             ; Create a local variable storing the RootID
    hatch-roots 1 [                          ; ... create a new turtle...
      set size r_root_t * size-factor * 2    ; ... with the size of the Root Radius...
      set color lput 100 color               ; ... and set the color to be a bit transparent.
      set RootNr who
    ]
    set RootID RootNr
  ]



end

;__________________________________________________________________________________________________________________________________________
;________________________________________________________________Go Procedures_____________________________________________________________
;__________________________________________________________________________________________________________________________________________

;----------------------------------------------------------------------
;--- Update any time-related variables.
;----------------------------------------------------------------------
to update-time
  set Time Time + 1                                              ; Go to the next Hour in the Climate Data Dataset.
  if Time >= (length ClimateData) [                              ; If the Climate Data Set is at its End...
    set Time 1                                                   ; ... reset it to Year 1, Day 1.
  ]
  set nRow item Time ClimateData
  let TimeFormat item 5 nRow                                     ; ... and Displayed Time (POSIXct-format).
  if TimeFormat < 10 [set TimeFormat (word 0 TimeFormat)]
  set POSIXct (word item 1 nRow " " TimeFormat ":00:00")
  set Month item 3 nRow                                          ; Set Month.

  if Day != item 4 nRow [                                        ; If a new day starts...
    set JulianDay JulianDay + 1                                  ; ... add one Day to the Julian Day (or Day of Year).
    ifelse JulianDay > nDays [
      set JulianDay 1
      set Year Year + 1
      ifelse is.divisible Year 4 [
        set nDays 366
      ] [
        set nDays 365
      ]
    ] [
    ]
    set Day item 4 nRow                                         ; Set Day.
  ]
  set Hour item 5 nRow                                          ; Set Hour.
  calc-Photosynthesis nRow
end

;----------------------------------------------------------------------
;--- Update if trees do photosynthesis
;----------------------------------------------------------------------
to calc-Photosynthesis [nRow_f]
  ifelse item 3 nRow_f > 4 AND item 3 nRow_f < 11 [    ; Trees grow between May and October. So if it's between May and October...
    ask trees [                                        ; ... for all trees...
      set Leaf? TRUE                                   ; ... set the period in which trees have leafs to True.
    ]
  ] [
    ask trees [                                        ; If its not between May and October, set the leafs False.
      set Leaf? FALSE
    ]
  ]
  ifelse item 8 nRow_f = 1 [                           ; Trees can only grow during the day. So if the sun is up...
    ask trees [                                        ; ... for all trees...
      ifelse Leaf? = TRUE [                            ; ... that have leafs...
        set Photo? TRUE                                ; set the photosynthesis activity True.
      ] [
        set Photo? FALSE                               ; If trees don't have leafs, trees cannot do photosynthesis
      ]
    ]
  ] [
    ask trees [                                        ; If the sun is not up, trees cannot do photosynthesis.
      set Photo? FALSE
    ]
  ]
end

;----------------------------------------------------------------------
;--- Update all climate variables.
;----------------------------------------------------------------------
to update-climate
  set Precipitation item 7 nRow                                  ; Set Precipitation [mm/m²]

  ; To calculate Interception and Stemflow, we need to define Precipitation Events.
  ; In here, precipitation events are split by a lack of precipitation for 4 hours or more.
  ; (Staelense et al. 2008, Xiao et al. 2000)
  ifelse Precipitation = 0 [                                     ; If no precipitation is occuring...
    set h_E_Precipitation h_E_Precipitation + 1                  ; ... set the time with no precipitation + 1 (hour).
    if h_E_Precipitation = 4 [                                   ; If no Precipitation occurred for 4 Hours (of h_E_Precip_needed Hours)...
      set h_E_Precipitation 0                                    ; ... reset the Amount of Hours since no Precipitation occurred.
      if E_Precipitation != 0 [                                  ; If there is any Event-Precipitation...
        ask patches [set c_infiltration E_Precipitation]         ; ... Save the Event-Precipitation as a Patches Variable.
        foreach sort-on [(- h_stem)] trees [                     ; Start with the biggest tree...
          the-tree -> ask the-tree [
            interception I1 I2                                   ; ... And let them reduce the amount of water that is reaching the Patches.
            stemflow                                             ; Then, calculate stemflow and add water to the patch where a tree is growing.
          ]
        ]
        ask patches [soil-water c_infiltration]                  ; Finally, add this reduced water to the patches.
        set E_Precipitation 0                                    ; Reset the Event-Precipitation.
      ]
    ]
  ] [
    set h_E_Precipitation 0                                      ; ... else (if it is indeed raining), set the time between precipitation events back to 0...
    set E_Precipitation E_Precipitation + Precipitation          ; ... and add the current precipitation [mm] to the total precipitation of that particular precipitation event.
  ]
  set t_precipitation t_precipitation + Precipitation            ; Record total precipitation.
  color-patches                                                  ; Color patches depending on their water availability.
end


;----------------------------------------------------------------------
;--- Calculate Interception Loss caused by Trees for each patch (in the radius of the tree crown)
;----------------------------------------------------------------------
to interception [I1_p I2_p]
  ifelse any? patches in-radius r_crown [                       ; Is there any patch (midpoint) in the radius of the tree crown? If yes:
    ask patches in-radius r_crown [                             ; Ask all Patches within the crown radius.
      ifelse c_infiltration < (I1_p ^ (1 / (-1 * I2_p))) [      ; If the Water Content is really low (too low to calculate it with our Function)...
        set l_interception 1                                    ; ... set the Interception Loss to 1 (no Water reaches the Ground)...
      ] [                                                       ; ... else...
        set l_interception (I1_p * (c_infiltration ^ I2_p))     ; ... calculate Interception Loss with a Function (Staelense et al. 2008).
      ]
      set c_infiltration c_infiltration * (1 - l_interception)  ; Reduce the amount of Infiltrating Water by the Interception Loss.
      set t_interception t_interception + (c_infiltration * l_interception / (count patches)) ; Add the Interception of that Patch to the total mean Interception.
    ]
  ] [                                                           ; If there is no patch (midpoint) in the radius of the tree crown, ...
    ask patch-here [                                            ; ... only alter the infiltration of the patch where the tree stands.
      ifelse c_infiltration < (I1_p ^ (1 / (-1 * I2_p))) [      ; If the Water Content is really low (too low to calculate it with our Function)...
        set l_interception 1                                    ; ... set the Interception Loss to 1 (no Water reaches the Ground)...
      ] [                                                       ; ... else...
        set l_interception (I1_p * (c_infiltration ^ I2_p))     ; ... calculate Interception Loss with a Function (Staelense et al. 2008).
      ]
      set c_infiltration c_infiltration * (1 - l_interception)  ; Reduce the amount of Infiltrating Water by the Interception Loss.
      set t_interception t_interception + (c_infiltration * l_interception / (count patches)) ; Add the Interception of that Patch to the total mean Interception.
    ]
  ]
end

;----------------------------------------------------------------------
;--- Calculate Stemflow. How much water are added to patches?
;----------------------------------------------------------------------
to stemflow
  if E_Precipitation > (-1 * S1) [                                          ; If the Event Precipitation is bigger than the minimum of Precipitation where Stemflow occurs...
    let c_stemflow S1 + S2 * E_Precipitation * (pi * (r_crown ^ 2))         ; ... calculate the stemflow using a function from Staelense et al. (2008).
    if c_stemflow < 0 [                                                     ; In case this stemflow is smaller than 0...
      set c_stemflow 0                                                      ; ... reset the stemflow to 0.
    ]
    ask patch-here [                                                        ; For the patch on which the tree stands...
      set c_water c_water + (c_stemflow / 1000)                             ; ... add the amount of stemflow-Water.
      if c_water > v_water_max [                                            ; If the the maximum Water Content is reached...
        set v_water_gravity (v_water_gravity + (c_water - v_water_max))     ; ... calculate the amount of water that is infiltrating into ground water...
        set t_gravity t_gravity + (((c_water - v_water_max) / (count patches)) * 1000)   ; Calculate the total mean Gravitational Water.
        set c_water v_water_max                                             ; ... and set the water content back to the maximum possible water content.
      ]
    ]
  ]
end

;----------------------------------------------------------------------
;--- Update Soil-Water-Content and Soil-Suction-Pressure.
;----------------------------------------------------------------------
to soil-water [MyPrecipitation]
  set c_water c_water + (MyPrecipitation / 1000)                        ; Calculate the Water Content per Patch.
  set p_infiltration p_infiltration + MyPrecipitation                   ; Document the infiltration amount.
  if c_water > v_water_max [                                            ; If the the maximum Water Content is reached...
    set v_water_gravity (v_water_gravity + (c_water - v_water_max))     ; ... calculate the amount of water that is infiltrating into ground water...
    set t_gravity t_gravity + (((c_water - v_water_max) / (count patches)) * 1000)   ; Calculate the total mean Gravitational Water.
    set c_water v_water_max                                             ; ... and set the water content back to the maximum possible water content.
  ]
  ifelse c_water > v_water_min [                                        ; If the water content is above 0.03 % (which is really low and plants can't use this)...
    ifelse matric-potential != "fixed" [                                ; ... and the matric-potential is "dynamic"...
      set psi_matrix (- 1 * (reverse-pF w_s w_r alpha n c_water))       ; ... calculate the energy (suction power) needed to extract this water...
    ] [
      set psi_matrix (- 1 * (10 ^ fixed-pF))                            ; ... and if the matric-potential is fixed, reset the matric-potential to the fixed potential...
    ]
  ] [
    set psi_matrix -7860000 ; pF 4.2 is usually used for pine as PWP (~ 1.5 MPa)  ; ... else set the matrix potential to the amount where plants cannot grow anymore
  ]
end

;----------------------------------------------------------------------
;--- Calculate light availability for each tree and patch
;----------------------------------------------------------------------
to shade
  ask trees [
    set RES_a solar_r * pi * r_crown ^ 2
  ]
;  ask patches [                                                   ; For all patches...
;    set p_light 1                                                 ; ... first set the patch light availability to 1 (full light).
;  ]
;  foreach sort-on [(- h_stem)] trees [                            ; Start with the highest trees...
;    the-tree -> ask the-tree [
;      let f_light t_light                                         ; ... create a local variable storing the transparency of the tree crown to use in a patch-context.
;      let light 1                                                 ; Set the tree light availability to 1 (full light)...
;      ifelse any? patches in-radius r_crown [                     ; ... and if there is any patch center in crown radius...
;        set light mean [p_light] of patches in-radius r_crown     ; ... set the tree light availability to the mean of the patch light availabilities...
;        ask patches in-radius r_crown [                           ; ... and reduce the light that reaches the ground (patch light availability)...
;          set p_light p_light * (1 - f_light)                     ; ... by the light taken by the crown.
;        ]
;      ] [                                                         ; If there is no patch center inside the crown radius (tree is really small)...
;        set light ([p_light] of patch-here)                       ; ... just use the light availability of the patch where the tree is standing...
;        ask patch-here [                                          ; ... and also reduce the light that reaches the ground of this patch.
;          set p_light p_light * (1 - f_light)
;        ]
;      ]
;      set RES_a solar_r * pi * r_crown ^ 2 * light                ; Then also calculate the above-ground resources [m³ water]
;    ]
;  ]
end

;----------------------------------------------------------------------
;--- Calculate Tree-Growth-Potential
;----------------------------------------------------------------------
to Potential-Growth
  Grow-Volume         ; Calculate the Volume according to the the new radii from last tick
  Resistance          ; Calculate sap flow resistance and root surface resistance
  Resources           ; Here the overall resources that the tree has (for growing, maintainance is already substracted) are calculated
end

;----------------------------------------------------------------------
;--- Calculate Volume of Tree
;----------------------------------------------------------------------
to Grow-Volume
  ; All volumes are calculated as [m³]
  set v_leaf       h_crown * pi * r_crown ^ 2
  set v_branch     r_crown * pi * r_stem  ^ 2 * 2
  set v_stem       h_stem  * pi * r_stem  ^ 2
  set v_cableroot  r_root  * pi * r_stem  ^ 2 * 2 ^ (-0.5)
  set v_fineroot   h_root  * pi * r_root  ^ 2
  set v_root       v_cableroot + v_fineroot
  set v_crown      v_leaf + v_branch
  set v_tree       v_crown + v_stem + v_root
end

;----------------------------------------------------------------------
;--- Set Sap flow resistance and root skin resistance
;----------------------------------------------------------------------
to Resistance
  set R1 1 / (L_p * k_geom * h_root * pi * r_root ^ 2)                                ; calculate root skin resistance [(s * Pa) / m³]    see Peters et al. 2014, Eq. 22
  set R2 (h_stem + 2 ^ (-0.5) * r_root + 2 * r_crown) / (k_f_sap * pi * r_stem ^ 2)   ; calculate sap flow resistance  [(s * Pa) / m³]    see Peters et al. 2014, Eq. 23
end

;----------------------------------------------------------------------
;--- Calculate Resource Availability
;----------------------------------------------------------------------
to Resources
  let psi_height -1000 * gravity * (h_stem + 2 * r_crown)    ; Water Potential loss due to tree height. Tree Height Potential [Pa] (Peters et al. 2014, Eq. 18, 20)
                                                             ; -1000 kg/m³ * 9.81 m/s * x m -> kg/(m*s²) -> Pa
  ; This is altered from the Original BETTINA-Model
  set RES_b (Calc-RESb r_root psi_leaf psi_height R1 R2)     ; Calculate the below ground growth restriction [m³ / s].

  set RES_avail (min (list (RES_a * k_rel) RES_b))           ; set the overall growth restriction to the lower one of above and below ground restriction. Above-ground resource availability has been calculated in the "shade" submodel.
  set RES_sum RES_sum + RES_avail                            ; Sum up the Water Usage. This is just for further investigations.
  set growth k_grow * (RES_avail - k_maint * v_tree)         ; Calculate Growth [m³] using the Ressource availability and substract the maintainence costs. Also scale the growth (k_grow ~= water use efficiency)
  if growth < 0 [set growth 0]
  set growth growth * 3600                                   ; Multiply (the growth per second) times (the amount of seconds). Because everything is calculated in SI-units, Growth is also calculated per Second. 3600s = 1h
end

;----------------------------------------------------------------------
;--- Calculate below-Ground Resource Availability
;----------------------------------------------------------------------
to-report Calc-RESb [.r_root .psi_leaf .psi_height .R1 .R2]
  let n_patches count patches in-radius .r_root                                        ; To how many Patches does a Tree have Access to?
  let RES_b_f 0                                                                        ; Set the Below-Ground-Growth-Restriction to 0 [m³ / s].
  ifelse any? patches in-radius .r_root [
    ask patches in-radius .r_root [                                                    ; For each Patch within the Root Radius of a Tree...
      let psi_gradient min (list (.psi_leaf - .psi_height - psi_matrix) 0)             ; ... Calculate the Pressure Gradient from each Patch to the Tree Crown [Pa].
      set RES_b_f RES_b_f + ((0 - psi_gradient / ((.R1 + .R2) * pi)) / (n_patches))    ; Sum up the Below-Ground-Growth-Restrictions, derived from each Pressure Gradient...
    ]                                                                                  ; ... of each Patch a Tree has Access to. This is devided by to total Number of Patches a Tree has Access to.
  ] [                                                                                  ; This means a Tree is pulling Water equally from each of the surrounding Patches.
    ask patch-here [
      let psi_gradient min (list (.psi_leaf - .psi_height - psi_matrix) 0)             ; If the Tree is still small, there might not be a Patch Core within the Root Radius, so just take the Patch the Tree is standing on.
      set RES_b_f RES_b_f + ((0 - psi_gradient / ((.R1 + .R2) * pi)))
    ]
  ]

  report RES_b_f
end

;----------------------------------------------------------------------
;--- Let Trees Take up Water.
;----------------------------------------------------------------------
to Water-Uptake
  let psi_height -1000 * gravity * (h_stem + 2 * r_crown)                                       ; Water Potential loss due to tree height. Tree Height Potential [Pa] (Peters et al. 2014, Eq. 18, 20)
  set v_water (Calc-Water-Uptake r_root psi_leaf psi_height R1 R2 RES_avail RES_b)              ; Calculate the Water uptake of a Tree.
  set t_uptake (t_uptake + (v_water * 1000 / (count patches)))                                  ; Sum up the Water uptake.
end

;----------------------------------------------------------------------
;--- Let Trees take up Water from Soil. Decrease Soil Water Content.
;----------------------------------------------------------------------
to-report Calc-Water-Uptake [.r_root .psi_leaf .psi_height .R1 .R2 .RES_avail .RES_b]
  let t_v_water 0                                                                               ; Reset the Tree Water Uptake per Hour to 0.
  let n_patches count patches in-radius .r_root                                                 ; To how many Patches does a Tree have Access to?
  let RES_ratio 0                                                                               ; Set the Ratio of of Below-Ground Ressource Availability to 0.
  if .RES_b > 0 [set RES_ratio .RES_avail / .RES_b]                                             ; How much of the possible water uptake is actually taken up?
  ifelse any? patches in-radius .r_root [
    ask patches in-radius .r_root [                                                             ; For each Patch within the Root Radius of a Tree...
      ifelse matric-potential != "fixed" [
        let psi_gradient min (list (.psi_leaf - .psi_height - psi_matrix) 0)                    ; ... Calculate the Pressure Gradient from each Patch to the Tree Crown [Pa].
        let l_water ((0 - psi_gradient / ((.R1 + .R2) * pi)) / (n_patches)) * 3600 * RES_ratio  ; Calculate the water loss of a single patch...
        if ((c_water - l_water) < w_r) [                                                        ; ... and make sure the Soil Water Content is not falling below the minimum Soil Water Content (w_r).
          set l_water c_water - w_r
        ]
        set c_water c_water - l_water                                                           ; Substract the Water Uptake from the Soil Water Content.
        set psi_matrix (- 100 * (reverse-pF w_s w_r alpha n c_water))
        set t_v_water t_v_water + l_water                                                       ; Add the Water, taken from a single Patch and add it to the total Water Uptake of one Tree
      ] [
        set psi_matrix -1 * (10 ^ fixed-pF)                                                     ; If the Soil-Suction-Pressure is fixed, set it to the fixed value.
      ]
    ]
  ] [                                                                                           ; If the Tree is still small, there might not be a Patch Core within the Root Radius, so just take the Patch the Tree is standing on.
    ask patch-here [
      ifelse matric-potential != "fixed" [
        let psi_gradient min (list (.psi_leaf - .psi_height - psi_matrix) 0)
        let l_water ((0 - psi_gradient / ((.R1 + .R2) * pi))) * 3600 * RES_ratio
        if ((c_water - l_water) < w_r) [
          set l_water w_r - c_water
          print c_water - l_water
        ]
        set c_water c_water - l_water
        ifelse c_water > w_r [
          set psi_matrix (- 100 * (reverse-pF w_s w_r alpha n c_water))
        ][
          set psi_matrix (- 100 * (reverse-pF w_s w_r alpha n (w_r * 1.00001)))
        ]

        set t_v_water t_v_water + l_water
      ] [
        set psi_matrix -1 * (10 ^ fixed-pF)
      ]
    ]
  ]

  report t_v_water                                                                              ; The Output is the Amount of Water the Tree is taking up.
end

;----------------------------------------------------------------------
;--- Calculate the Growth Allocation of the Tree. Where does the Tree Grow?
;----------------------------------------------------------------------
to Allocation
  ; Sigmoid functions are calculated (see Peters et al. 2014, Fig. 3). On the X-Axis these Q-Values are used as input.
  ; If Q is 0, then the restrictions (resistance, radii) are equal (in equilibrium). If they are shifting to -1/1 one is bigger than the other
  ; which means that the tree will grow in the direction of the restriction to widen that restriction.
  let Q_res (k_rel * RES_a - RES_b) / (k_rel * RES_a + RES_b)  ; Compare Above- and Below-Ground Resource Availability. Is used for Crown Growth
  let Q_r (R1 - R2) / (R1 + R2)                                ; Compare Root Surface Resistance to Sap Flow Resistance. Is used for Stem Growth. The Water Flow Resistance restricts Growth. Is the Root or Sap Flow Resistance higher?
  let Q_rad (r_crown - r_root) / (r_crown + r_root)            ; Compare Crown Radius to Root Radius. Is used for Stem Height Growth.

  ; In the sigmoid function the numerator is the maximum value possible (something between 0 and 1, 1 meaning that all growth is allocated to this part of the plant,
  ; 0 meaning that no growth is allocated to this part of the plant). In the denumerator the previously calculated Q values are used and a variable describing the
  ; maximum slope of the function.
  set w_h_stem  (max (list (                     h_max / (1 + exp((0 - Q_rad) / SigmoidSlopeHeight))) 0))  ; Peters et al. 2014, Eq. ??
  set w_r_crown (max (list (            (1 - w_h_stem) / (1 + exp(Q_res       / SigmoidSlopeRadius))) 0))  ; Peters et al. 2014, Eq. 27, 28
  set w_r_stem  (max (list ((1 - w_h_stem - w_r_crown) / (1 + exp(Q_r         / SigmoidSlopeRadius))) 0))  ; Peters et al. 2014, Eq. 29
  set w_r_root  (1 - w_h_stem - w_r_crown - w_r_stem)                                                      ; Peters et al. 2014, Eq. 26, 30

  ; For Growth the weighted growth [m³] is used and devided by the area [m²] of tree that grows (e.g. Stem Area) [m]
  set g_h_stem  (w_h_stem  * growth / (pi * r_stem ^ 2))                                                 ; Stem Height Growth  [m]
  set g_r_stem  (w_r_stem  * growth / (2 * pi * r_stem * (h_stem + 2 ^ (-0.5) * r_root + 2 * r_crown)))  ; Stem Radius Growth  [m]
  set g_r_crown (w_r_crown * growth / (2 * pi * (r_crown * h_crown + r_stem ^ 2)))                       ; Crown Radius Growth [m]
  set g_r_root  (w_r_root  * growth / (2 * pi * r_root * h_root + 2 ^ (-0.5) * pi * r_stem ^ 2))         ; Root Radius Growth  [m]
end

;----------------------------------------------------------------------
;--- Let the Tree finally Grow.
;----------------------------------------------------------------------
to Grow
  set h_stem  h_stem  + g_h_stem             ; ... in Stem Height  [m]
  set r_stem  r_stem  + g_r_stem             ; ... in Stem Radius  [m]
  set r_crown r_crown + g_r_crown            ; ... in Crown Radius [m]
  set r_root  r_root  + g_r_root             ; ... in Root Radius  [m]
  set size    (r_crown * 2 * size-factor)    ; ... in the UI-Size  [m]
  if  size < Min_Size [                      ; If the Size in the UI is smaller than the minimum Size in the UI...
    set size Min_Size                        ; Then set the Size to the minimum Size possible to be displayed in the UI.
  ]
  Grow-Volume                                ; Grow in Volume.
  if Plot-Roots? [                           ; If the Roots are Plotted as well...
    let r_root_t r_root                      ; ... save the root radius in a local variable...
    ask root rootID [                        ; ... and ask the corresponding Root...
      set size r_root_t * size-factor * 2    ; ... to change its Size.
    ]
  ]
end


;__________________________________________________________________________________________________________________________________________
;__________________________________________________________________________________________________________________________________________
;_________________________________________________________________Functions________________________________________________________________
;__________________________________________________________________________________________________________________________________________
;__________________________________________________________________________________________________________________________________________

to-report pF [.w_s .w_r .alpha .n .x]
  report (.w_r + ((.w_s - .w_r) / ((1 + (.alpha * .x) ^ .n) ^ (1 - (1 / .n)))))   ; Pay attention, this x_f / 100 was hPa before, so maybe something wrong
end


to-report reverse-pF [.w_s .w_r .alpha .n .y]
  report (((((.w_s - .w_r) / (.y - .w_r)) ^ (1 / (1 - 1 / .n)) - 1) ^ (1 / .n)) / .alpha)
end


to-report is.divisible [TheValue By]
  report (floor (TheValue / By)) = (TheValue / By)
end


; Make a Data Frame with ncol numbers of columns from a single List (InList).
to-report data.frame [InList ncol]
  let outdata (list)
  let xstart 0
  repeat ((length InList) / ncol) [
    set outdata lput (sublist InList (xstart) (xstart + ncol)) outdata
    set xstart xstart + ncol
  ]
  report outdata
end





;__________________________________________________________________________________________________________________________________________
;__________________________________________________________________________________________________________________________________________
;_________________________________________________________________Literature_______________________________________________________________
;__________________________________________________________________________________________________________________________________________
;__________________________________________________________________________________________________________________________________________

; MLUV (Ministerium für Ländliche Entwicklung, Umwelt und Verbraucherschutz des Landes Brandenburg) (2006), Bestandeszieltypen für die Wälder des Landes Brandenburg, Potsdam
; Staelends, Joerens; De Schrijver, An; Verheyen, Kris; Verhoest, Niko E. C. (2008), Rainfall partitioning into throughfall, stemflow and interception within a single beech (Fagus sylvatica L.) canopy. DOI: 10.1002/hyp.6610
; Xiao, Qingfu; McPherson, E. Gregory; Ustin, Susan L.; Grismer, Mark E.; Simpson, James R. (2000), Winter rainfall interception by two mature open-grown trees in Davis, California. https://doi.org/10.1002/(SICI)1099-1085(200003)14:4<763::AID-HYP971>3.0.CO;2-7
; Peters, Ronny; Vovides, Alejandra G.; Luna, Soledad; Grüters, Uwe; Berger, Uta (2014), Changes in allometric relations of mangrove trees due to resource availability - A new mechanistic modelling approach. DOI: 10.1016/j.ecolmodel.2014.04.001
@#$#@#$#@
GRAPHICS-WINDOW
729
10
1196
478
-1
-1
9.0
1
10
1
1
1
0
1
1
1
-25
25
-25
25
0
0
1
ticks
30.0

BUTTON
49
66
112
99
setup
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
114
66
177
99
go
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
179
66
271
99
go-forever
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
49
101
167
134
Overstory?
Overstory?
0
1
-1000

SWITCH
169
101
305
134
Regeneration?
Regeneration?
0
1
-1000

CHOOSER
49
136
187
181
Matric-Potential
Matric-Potential
"fixed" "dynamic"
0

SLIDER
189
136
361
169
fixed-pF
fixed-pF
0
7
2.0
0.1
1
NIL
HORIZONTAL

CHOOSER
48
225
186
270
Record-Interval
Record-Interval
"hour" "day" "year"
0

SWITCH
188
225
345
258
Record-Regeneration?
Record-Regeneration?
1
1
-1000

SWITCH
188
260
345
293
Record-Overstory?
Record-Overstory?
1
1
-1000

SWITCH
188
295
345
328
Record-Soil?
Record-Soil?
1
1
-1000

INPUTBOX
48
330
345
390
Output-file
NIL
1
0
String

SLIDER
541
163
713
196
w_s
w_s
0
1
0.488
0.001
1
[m³ / m³]
HORIZONTAL

SLIDER
541
198
713
231
w_r
w_r
0
1
0.028
0.028
1
[m³ / m³]
HORIZONTAL

SLIDER
541
233
713
266
alpha
alpha
0
1
0.051
0.001
1
[1 / Pa]
HORIZONTAL

SLIDER
541
268
713
301
n
n
0
10
1.544
0.001
1
[-]
HORIZONTAL

SLIDER
541
303
713
336
FK
FK
0
3
1.8
0.1
1
[pF]
HORIZONTAL

PLOT
1199
10
1472
237
Water-Retention-Curve
Soil Water Volume [Vol-%]
Soil suction [pF-Value]
0.0
100.0
0.0
7.0
false
false
"" ""
PENS
"Water-Retention-Curve" 1.0 0 -16777216 true "" ""

CHOOSER
532
52
686
97
Patches-Color
Patches-Color
"Soil Water Content" "Soil Suction"
0

SWITCH
532
99
651
132
Plot-Roots?
Plot-Roots?
0
1
-1000

MONITOR
729
481
862
526
Date
POSIXct
0
1
11

PLOT
696
546
1194
809
Ressource Availability
time
Resource Availability [m³]
0.0
10.0
0.0
1.01E-7
true
true
"" ""
PENS
"above-ground" 1.0 0 -15040220 true "" "if any? turtles [plot mean [RES_a] of trees]"
"below-ground" 1.0 0 -10402772 true "" "if any? turtles [plot mean [RES_b] of trees]"

PLOT
1211
527
1503
742
plot 1
NIL
NIL
0.0
10.0
0.0
0.05
true
true
"" ""
PENS
"a_crown" 1.0 0 -13210332 true "" "if any? trees [plot mean [pi * (r_crown ^ 2)] of trees with [Layer = \"Regeneration\"]]"
"a_root" 1.0 0 -10402772 true "" "if any? trees [plot mean [pi * (r_root ^ 2)] of trees with [Layer = \"Regeneration\"]]"

PLOT
1609
550
1809
700
plot 2
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "if any? trees [plot mean [h_stem] of trees with [Layer = \"Regeneration\"]]"

PLOT
1291
323
1585
473
plot 3
NIL
NIL
0.0
10.0
0.0
1.0
true
true
"" ""
PENS
"w_h_stem" 1.0 0 -12345184 true "" "if any? trees [plot mean [w_h_stem] of trees with [Layer = \"Regeneration\"]]"
"w_r_stem" 1.0 0 -3844592 true "" "if any? trees [plot mean [w_r_stem] of trees with [Layer = \"Regeneration\"]]"
"w_r_crown" 1.0 0 -13210332 true "" "if any? trees [plot mean [w_r_crown] of trees with [Layer = \"Regeneration\"]]"
"w_r_root" 1.0 0 -10402772 true "" "if any? trees [plot mean [w_r_root] of trees with [Layer = \"Regeneration\"]]"

PLOT
1632
371
1832
521
plot 4
NIL
NIL
0.0
10.0
0.0
0.01
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "if any? trees [plot mean [r_stem] of trees with [Layer = \"Regeneration\"]]"

SLIDER
307
101
479
134
SimYears
SimYears
0
50
5.0
1
1
NIL
HORIZONTAL

BUTTON
307
66
400
99
go-x-years
repeat simYears [\nifelse (remainder (item 2 item time ClimateData) 4) = 0 [\nrepeat round (366 * 24) [go]\n][\nrepeat round (365 * 24) [go]\n]\n]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

@#$#@#$#@
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

circle3
true
0
Circle -7500403 false true 0 0 300

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

mytree
true
0
Circle -7500403 false true 103 103 94
Circle -7500403 false true 108 108 85
Circle -7500403 false true 105 105 90
Circle -7500403 false true 103 103 94
Circle -7500403 false true 107 107 86
Circle -7500403 false true 108 108 84
Circle -7500403 false true 96 96 108
Circle -7500403 false true 98 98 104
Circle -7500403 false true 100 100 100
Circle -7500403 false true 101 101 98
Circle -7500403 false true 106 106 88
Circle -7500403 false true 105 105 90
Circle -7500403 false true 100 100 100
Circle -7500403 false true 102 102 96
Circle -7500403 false true 98 98 104
Circle -7500403 false true 103 103 94
Circle -7500403 false true 97 97 106
Circle -7500403 false true 97 97 106
Circle -7500403 false true 99 99 102
Circle -7500403 false true 104 104 92

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
NetLogo 6.2.2
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
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
