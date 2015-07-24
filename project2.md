# Effects of Severe Weather Events Across America
Calvin Seto  
July 6, 2015  

# Synopsis
This document attempts to identify the most catastrophic weather events America has faced during the past 60 years and the impact on the population health and economic well-being.  The Storm Events Database contains information from 1950 to November 2011 and has undergone major changes in order to fix data inconsistencies.  It's expected that some effort will be necessary to clean and tidy data set variables such as event type, fatalities, injuries, property damage, property damage units, crop damage, and crop damage units.  After creating the appropriate subsets, we'll create dotplots for health, fatalities, injuries, economic, property, and crop damage for each of the 48 permissible event types.
The top three event types for health were one, two, and three.
The top three event types for fatalities were one, two, and three.
The top three event types for injuries were one, two, and three.
The top three event types for economic damage were one, two, and three.
The top three event types for property damage were one, two, and three.
The top three event types for crop damage were one, two, and three.

# Data Processing
The data can be downloaded from the [project web site](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2) as a bzip compressed file and it is saved to the local disk as StormData.csv.bz2, in the working directory's data folder.  We'll load the CSV file with read.csv() and use bzfile() to access the bzip file without uncompressing it.  To make grouping and summarizing easier, we'll configure dplyr.

```r
mydf <- read.csv(bzfile("./data/StormData.csv.bz2"),header=TRUE,sep=",")
library(dplyr)
storm <- tbl_df(mydf)
rm("mydf")
```

# Data Exploration
As we explore the data set, we see its size, the first few rows of data, and identify variables that can help us answer our questions: BGN_DATE, COUNTY, STATE, EVTYPE, END_DATE, FATALITIES, INJURIES, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP, REMARKS, and REFNUM.

To see how many events were recorded in the Storm Database, let's create a table of event counts by year from 1950 to 2011.
We create the 5 number summary and a table of the number of weather events counted over the years.

```r
dim(storm)
```

```
## [1] 902297     37
```

```r
head(storm)
```

```
##   STATE__           BGN_DATE BGN_TIME TIME_ZONE COUNTY COUNTYNAME STATE
## 1       1  4/18/1950 0:00:00     0130       CST     97     MOBILE    AL
## 2       1  4/18/1950 0:00:00     0145       CST      3    BALDWIN    AL
## 3       1  2/20/1951 0:00:00     1600       CST     57    FAYETTE    AL
## 4       1   6/8/1951 0:00:00     0900       CST     89    MADISON    AL
## 5       1 11/15/1951 0:00:00     1500       CST     43    CULLMAN    AL
## 6       1 11/15/1951 0:00:00     2000       CST     77 LAUDERDALE    AL
##    EVTYPE BGN_RANGE BGN_AZI BGN_LOCATI END_DATE END_TIME COUNTY_END
## 1 TORNADO         0                                               0
## 2 TORNADO         0                                               0
## 3 TORNADO         0                                               0
## 4 TORNADO         0                                               0
## 5 TORNADO         0                                               0
## 6 TORNADO         0                                               0
##   COUNTYENDN END_RANGE END_AZI END_LOCATI LENGTH WIDTH F MAG FATALITIES
## 1         NA         0                      14.0   100 3   0          0
## 2         NA         0                       2.0   150 2   0          0
## 3         NA         0                       0.1   123 2   0          0
## 4         NA         0                       0.0   100 2   0          0
## 5         NA         0                       0.0   150 2   0          0
## 6         NA         0                       1.5   177 2   0          0
##   INJURIES PROPDMG PROPDMGEXP CROPDMG CROPDMGEXP WFO STATEOFFIC ZONENAMES
## 1       15    25.0          K       0                                    
## 2        0     2.5          K       0                                    
## 3        2    25.0          K       0                                    
## 4        2     2.5          K       0                                    
## 5        2     2.5          K       0                                    
## 6        6     2.5          K       0                                    
##   LATITUDE LONGITUDE LATITUDE_E LONGITUDE_ REMARKS REFNUM
## 1     3040      8812       3051       8806              1
## 2     3042      8755          0          0              2
## 3     3340      8742          0          0              3
## 4     3458      8626          0          0              4
## 5     3412      8642          0          0              5
## 6     3450      8748          0          0              6
```

```r
names(storm)
```

```
##  [1] "STATE__"    "BGN_DATE"   "BGN_TIME"   "TIME_ZONE"  "COUNTY"    
##  [6] "COUNTYNAME" "STATE"      "EVTYPE"     "BGN_RANGE"  "BGN_AZI"   
## [11] "BGN_LOCATI" "END_DATE"   "END_TIME"   "COUNTY_END" "COUNTYENDN"
## [16] "END_RANGE"  "END_AZI"    "END_LOCATI" "LENGTH"     "WIDTH"     
## [21] "F"          "MAG"        "FATALITIES" "INJURIES"   "PROPDMG"   
## [26] "PROPDMGEXP" "CROPDMG"    "CROPDMGEXP" "WFO"        "STATEOFFIC"
## [31] "ZONENAMES"  "LATITUDE"   "LONGITUDE"  "LATITUDE_E" "LONGITUDE_"
## [36] "REMARKS"    "REFNUM"
```

```r
library(lubridate)
totalEventsByYear <- as.data.frame(table(year(mdy_hms(storm$BGN_DATE))))
names(totalEventsByYear) <- c("year","count")
summary(totalEventsByYear$count)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##     223    2388    5766   14550   28500   62170
```

The str() function run on the EVTYPE variable shows it is quite messy being a Factor variable with 985 levels.
Let's do a count of the event types in the database and view the types in decreasing count order.  There are too many messy values for EVTYPE, so we'll reclassify them using the 48 allowed values for storm data events specified in the NWSI 10-1065 document.  The idea is to create a new factor variable using a custom function makeCleanEventType() which will examine each messy EVTYPE value and map it to one of the 48 allowed values.  The strings in this function were created using the helpful features of text editors like TextMate or Notepad++, and dplyr query results.  The new factor variable is called storm$cleanEventType.  To help calculate property and crop damage later, we'll create two variables called cleanPROPDMG and cleanCROPDMG.


```r
str(storm$EVTYPE)
```

```
##  Factor w/ 985 levels "   HIGH SURF ADVISORY",..: 834 834 834 834 834 834 834 834 834 834 ...
```

```r
countEventTypes <- as.data.frame(table(storm$EVTYPE))
names(countEventTypes) <- c("eventType","count")
countEventTypes[order(countEventTypes$count,decreasing=TRUE),]
```

```
##                          eventType  count
## 244                           HAIL 288661
## 856                      TSTM WIND 219940
## 760              THUNDERSTORM WIND  82563
## 834                        TORNADO  60652
## 153                    FLASH FLOOD  54277
## 170                          FLOOD  25326
## 786             THUNDERSTORM WINDS  20843
## 359                      HIGH WIND  20212
## 464                      LIGHTNING  15754
## 310                     HEAVY SNOW  15708
## 290                     HEAVY RAIN  11723
## 972                   WINTER STORM  11433
## 978                 WINTER WEATHER   7026
## 216                   FUNNEL CLOUD   6839
## 490               MARINE TSTM WIND   6175
## 489       MARINE THUNDERSTORM WIND   5812
## 936                     WATERSPOUT   3796
## 676                    STRONG WIND   3566
## 919           URBAN/SML STREAM FLD   3392
## 957                       WILDFIRE   2761
## 30                        BLIZZARD   2719
## 95                         DROUGHT   2488
## 427                      ICE STORM   2006
## 130                 EXCESSIVE HEAT   1678
## 376                     HIGH WINDS   1533
## 955               WILD/FOREST FIRE   1457
## 212                   FROST/FREEZE   1342
## 89                       DENSE FOG   1293
## 980             WINTER WEATHER/MIX   1104
## 873                 TSTM WIND/HAIL   1028
## 141        EXTREME COLD/WIND CHILL   1002
## 275                           HEAT    767
## 350                      HIGH SURF    725
## 848                 TROPICAL STORM    690
## 164                 FLASH FLOODING    682
## 140                   EXTREME COLD    655
## 54                   COASTAL FLOOD    650
## 440               LAKE-EFFECT SNOW    636
## 177              FLOOD/FLASH FLOOD    624
## 442                      LANDSLIDE    600
## 629                           SNOW    587
## 79                 COLD/WIND CHILL    539
## 188                            FOG    538
## 585                    RIP CURRENT    470
## 485                    MARINE HAIL    442
## 117                     DUST STORM    427
## 19                       AVALANCHE    386
## 960                           WIND    340
## 586                   RIP CURRENTS    304
## 670                    STORM SURGE    261
## 201                  FREEZING RAIN    250
## 905                    URBAN FLOOD    249
## 342           HEAVY SURF/HIGH SURF    228
## 146              EXTREME WINDCHILL    204
## 680                   STRONG WINDS    196
## 101                 DRY MICROBURST    186
## 17           ASTRONOMICAL LOW TIDE    174
## 402                      HURRICANE    174
## 590                    RIVER FLOOD    173
## 458                     LIGHT SNOW    154
## 671               STORM SURGE/TIDE    148
## 578                  RECORD WARMTH    146
## 57                COASTAL FLOODING    143
## 115                     DUST DEVIL    141
## 486               MARINE HIGH WIND    135
## 886              UNSEASONABLY WARM    126
## 185                       FLOODING    120
## 16          ASTRONOMICAL HIGH TIDE    103
## 505              MODERATE SNOWFALL    101
## 908                 URBAN FLOODING     98
## 984                     WINTRY MIX     90
## 411              HURRICANE/TYPHOON     88
## 219                  FUNNEL CLOUDS     87
## 339                     HEAVY SURF     84
## 557                    RECORD HEAT     81
## 192                         FREEZE     74
## 278                      HEAT WAVE     74
## 66                            COLD     72
## 550                    RECORD COLD     64
## 417                            ICE     61
## 801        THUNDERSTORM WINDS HAIL     61
## 847            TROPICAL DEPRESSION     60
## 608                          SLEET     59
## 884               UNSEASONABLY DRY     56
## 210                          FROST     53
## 243                    GUSTY WINDS     53
## 816            THUNDERSTORM WINDSS     51
## 488             MARINE STRONG WIND     48
## 532                          OTHER     48
## 617                     SMALL HAIL     47
## 214                         FUNNEL     46
## 198                   FREEZING FOG     45
## 753                   THUNDERSTORM     45
## 747             Temperature record     43
## 861                TSTM WIND (G45)     39
## 56                Coastal Flooding     38
## 944                    WATERSPOUTS     37
## 506          MONTHLY PRECIPITATION     36
## 970                          WINDS     36
## 503            MIXED PRECIPITATION     34
## 636                   SNOW AND ICE     33
## 167                   FLASH FLOODS     32
## 222                          GLAZE     32
## 320             HEAVY SNOW SQUALLS     32
## 628                           Snow     30
## 916       URBAN/SMALL STREAM FLOOD     30
## 260                        HAIL 75     29
## 435                      ICY ROADS     28
## 966                    WIND DAMAGE     27
## 305                    HEAVY RAINS     26
## 135                 EXCESSIVE SNOW     25
## 283                HEAVY LAKE SNOW     25
## 592                 RIVER FLOODING     24
## 812        THUNDERSTORM WINDS/HAIL     24
## 237                     GUSTY WIND     23
## 441                LAKESHORE FLOOD     23
## 455            LIGHT FREEZING RAIN     23
## 606           SEVERE THUNDERSTORMS     23
## 881              UNSEASONABLY COLD     23
## 142                   EXTREME HEAT     22
## 161              FLASH FLOOD/FLOOD     22
## 926                   VOLCANIC ASH     22
## 438               LAKE EFFECT SNOW     21
## 457                     Light Snow     21
## 602                         SEICHE     21
## 195               FREEZING DRIZZLE     20
## 833                 TIDAL FLOODING     20
## 877                        TSUNAMI     20
## 147 EXTREME WINDCHILL TEMPERATURES     19
## 645                    SNOW SQUALL     19
## 836                     TORNADO F0     19
## 891               UNSEASONABLY WET     19
## 977                 Winter Weather     19
## 245                      HAIL 0.75     18
## 963                     WIND CHILL     18
## 536                   PROLONG COLD     17
## 648                   SNOW SQUALLS     17
## 658                 SNOW/ICE STORM     17
## 289                     Heavy Rain     16
## 540                           RAIN     16
## 321             HEAVY SNOW-SQUALLS     15
## 29                       BLACK ICE     14
## 567                RECORD RAINFALL     14
## 820            THUNDERSTORMS WINDS     14
## 96          DROUGHT/EXCESSIVE HEAT     13
## 252                       HAIL 100     13
## 255                       HAIL 175     13
## 604            SEVERE THUNDERSTORM     13
## 888      UNSEASONABLY WARM AND DRY     13
## 44                    BLOWING SNOW     12
## 610                    SLEET STORM     12
## 882              UNSEASONABLY COOL     12
## 961                  WIND ADVISORY     12
## 221                          Glaze     11
## 508               MONTHLY RAINFALL     11
## 571             Record temperature     11
## 627                          SMOKE     11
## 642             SNOW FREEZING RAIN     11
## 879                        TYPHOON     11
## 65                            Cold     10
## 90                     DENSE SMOKE     10
## 242                    Gusty Winds     10
## 501                   MIXED PRECIP     10
## 661                     SNOW/SLEET     10
## 860                TSTM WIND (G40)     10
## 885               UNSEASONABLY HOT     10
## 894                 UNUSUAL WARMTH     10
## 939                    WATERSPOUT-     10
## 98                             DRY      9
## 152                     FIRST SNOW      9
## 206            FREEZING RAIN/SLEET      9
## 306           HEAVY RAINS/FLOODING      9
## 349                      High Surf      9
## 408                 HURRICANE OPAL      9
## 518                       MUDSLIDE      9
## 60                   COASTAL STORM      8
## 114                     Dust Devil      8
## 165           FLASH FLOODING/FLOOD      8
## 228                 GRADIENT WINDS      8
## 284                      HEAVY MIX      8
## 348                      HIGH SEAS      8
## 444                     LANDSLIDES      8
## 517                       Mudslide      8
## 568                    RECORD SNOW      8
## 577                  Record Warmth      8
## 896                 UNUSUALLY COLD      8
## 914             URBAN/SMALL STREAM      8
## 943             WATERSPOUT/TORNADO      8
## 958                      WILDFIRES      8
## 200                  Freezing Rain      7
## 273                    HARD FREEZE      7
## 405                 HURRICANE ERIN      7
## 480                LOW TEMPERATURE      7
## 513                      MUD SLIDE      7
## 524                NON SEVERE HAIL      7
## 622             SMALL STREAM FLOOD      7
## 641                   SNOW DROUGHT      7
## 652              SNOW/BLOWING SNOW      7
## 657                       SNOW/ICE      7
## 679                   Strong Winds      7
## 754            THUNDERSTORM  WINDS      7
## 803   THUNDERSTORM WINDS LIGHTNING      7
## 14             AGRICULTURAL FREEZE      6
## 53                   Coastal Flood      6
## 70                  Cold and Frost      6
## 78    COLD WIND CHILL TEMPERATURES      6
## 87                 DAMAGING FREEZE      6
## 99                  DRY CONDITIONS      6
## 143             EXTREME WIND CHILL      6
## 182               FLOOD/RAIN/WINDS      6
## 202        FREEZING RAIN AND SLEET      6
## 231                       GUSTNADO      6
## 249                      HAIL 1.00      6
## 356                     HIGH WATER      6
## 369             HIGH WIND/BLIZZARD      6
## 570                RECORD SNOWFALL      6
## 643                   SNOW SHOWERS      6
## 654             SNOW/FREEZING RAIN      6
## 662       SNOW/SLEET/FREEZING RAIN      6
## 819             THUNDERSTORMS WIND      6
## 874                     TSTM WINDS      6
## 902 URBAN AND SMALL STREAM FLOODIN      6
## 947                 WET MICROBURST      6
## 959                           Wind      6
## 979             WINTER WEATHER MIX      6
## 58        COASTAL FLOODING/EROSION      5
## 107           DRY MICROBURST WINDS      5
## 126                 EARLY SNOWFALL      5
## 133                 EXCESSIVE RAIN      5
## 155      FLASH FLOOD FROM ICE JAMS      5
## 215                   Funnel Cloud      5
## 235       GUSTY THUNDERSTORM WINDS      5
## 331                 HEAVY SNOW/ICE      5
## 353                    HIGH SWELLS      5
## 392                HIGH WINDS/COLD      5
## 423               ICE JAM FLOODING      5
## 493                     MICROBURST      5
## 494               MICROBURST WINDS      5
## 520                      Mudslides      5
## 535                   Prolong Cold      5
## 544                      RAIN/SNOW      5
## 553                    RECORD COOL      5
## 560                    RECORD HIGH      5
## 572             RECORD TEMPERATURE      5
## 591                 River Flooding      5
## 595            ROTATING WALL CLOUD      5
## 605      SEVERE THUNDERSTORM WINDS      5
## 615                     small hail      5
## 625       SMALL STREAM/URBAN FLOOD      5
## 666              SNOWMELT FLOODING      5
## 832                 Tidal Flooding      5
## 866                   TSTM WIND 52      5
## 931                     WALL CLOUD      5
## 5                        TSTM WIND      4
## 10                 ABNORMAL WARMTH      4
## 13            ACCUMULATED SNOWFALL      4
## 41                    BLOWING DUST      4
## 67                 COLD AIR FUNNEL      4
## 75               COLD TEMPERATURES      4
## 77                    COLD WEATHER      4
## 84                       DAM BREAK      4
## 110                      DRY SPELL      4
## 111                    DRY WEATHER      4
## 134             EXCESSIVE RAINFALL      4
## 148            EXTREME/RECORD COLD      4
## 207             FREEZING RAIN/SNOW      4
## 209                          Frost      4
## 226                  Gradient wind      4
## 250                      HAIL 1.75      4
## 292            Heavy Rain and Wind      4
## 303                HEAVY RAIN/WIND      4
## 352             HIGH SURF ADVISORY      4
## 421                        ICE JAM      4
## 434                      Icy Roads      4
## 492                     Microburst      4
## 511            MONTHLY TEMPERATURE      4
## 521                      MUDSLIDES      4
## 531                          Other      4
## 538                 PROLONG WARMTH      4
## 539                 PROLONGED RAIN      4
## 563                     RECORD LOW      4
## 597                     ROUGH SURF      4
## 623          SMALL STREAM FLOODING      4
## 639                 SNOW AND SLEET      4
## 647                   Snow Squalls      4
## 768       THUNDERSTORM WIND 60 MPH      4
## 773          THUNDERSTORM WIND G50      4
## 782       THUNDERSTORM WIND/ TREES      4
## 818                  THUNDERSTORMS      4
## 837                     TORNADO F1      4
## 898                 UNUSUALLY WARM      4
## 917    URBAN/SMALL STREAM FLOODING      4
## 948                      Wet Month      4
## 951                       Wet Year      4
## 954                     WILD FIRES      4
## 965                    Wind Damage      4
## 22                   BEACH EROSION      3
## 27  BITTER WIND CHILL TEMPERATURES      3
## 28                       Black Ice      3
## 43                    Blowing Snow      3
## 49                      BRUSH FIRE      3
## 76                       COLD WAVE      3
## 124                     EARLY SNOW      3
## 172                    FLOOD FLASH      3
## 187                         FLOODS      3
## 194               Freezing Drizzle      3
## 199                  Freezing rain      3
## 227                  GRADIENT WIND      3
## 234        GUSTY THUNDERSTORM WIND      3
## 258                       HAIL 275      3
## 269                      HAIL/WIND      3
## 271                      HAILSTORM      3
## 288                     Heavy rain      3
## 304                 HEAVY RAINFALL      3
## 322            HEAVY SNOW/BLIZZARD      3
## 338                     Heavy Surf      3
## 354        HIGH TEMPERATURE RECORD      3
## 357                     HIGH WAVES      3
## 371           HIGH WIND/HEAVY SNOW      3
## 395                HIGH WINDS/SNOW      3
## 410     HURRICANE-GENERATED SWELLS      3
## 415           Hypothermia/Exposure      3
## 416           HYPOTHERMIA/EXPOSURE      3
## 431                       ICE/SNOW      3
## 460            Light Snow/Flurries      3
## 463                       LIGHTING      3
## 483                    MAJOR FLOOD      3
## 500                 MINOR FLOODING      3
## 502            Mixed Precipitation      3
## 529           NORMAL PRECIPITATION      3
## 533               PATCHY DENSE FOG      3
## 549                    Record Cold      3
## 561        RECORD HIGH TEMPERATURE      3
## 574            RECORD TEMPERATURES      3
## 579             Record Winter Snow      3
## 580          RECORD/EXCESSIVE HEAT      3
## 596                     ROUGH SEAS      3
## 673                   STREET FLOOD      3
## 674                STREET FLOODING      3
## 675                    Strong Wind      3
## 806            THUNDERSTORM WINDS.      3
## 823            THUNDERSTORMW WINDS      3
## 827              THUNDERTORM WINDS      3
## 838                     TORNADO F2      3
## 852           TROPICAL STORM JERRY      3
## 854                TSTM HEAVY RAIN      3
## 867                   TSTM WIND 55      3
## 900         URBAN AND SMALL STREAM      3
## 901   URBAN AND SMALL STREAM FLOOD      3
## 909                   URBAN FLOODS      3
## 921          URBAN/STREET FLOODING      3
## 928               VOLCANIC ASHFALL      3
## 967                     WIND GUSTS      3
## 971                     WINTER MIX      3
## 976                  WINTER STORMS      3
## 982                     Wintry mix      3
## 11                  ABNORMALLY DRY      2
## 24                     BEACH FLOOD      2
## 25      BELOW NORMAL PRECIPITATION      2
## 31  BLIZZARD AND EXTREME WIND CHIL      2
## 36             BLIZZARD/HEAVY SNOW      2
## 42                    blowing snow      2
## 45  BLOWING SNOW & EXTREME WIND CH      2
## 55                coastal flooding      2
## 59                   Coastal Storm      2
## 61                   COASTAL SURGE      2
## 62             COASTAL/TIDAL FLOOD      2
## 68                COLD AIR FUNNELS      2
## 74                Cold Temperature      2
## 83           CSTL FLOODING/EROSION      2
## 86                 Damaging Freeze      2
## 91                       DOWNBURST      2
## 92                 DOWNBURST WINDS      2
## 104              DRY MICROBURST 58      2
## 125                 Early snowfall      2
## 127             Erosion/Cstl Flood      2
## 129                 Excessive Cold      2
## 139                   Extreme Cold      2
## 150               FALLING SNOW/ICE      2
## 154       FLASH FLOOD - HEAVY RAIN      2
## 159             FLASH FLOOD/ FLOOD      2
## 171             FLOOD & HEAVY RAIN      2
## 175                    FLOOD/FLASH      2
## 178           FLOOD/FLASH FLOODING      2
## 191                         Freeze      2
## 223                      GLAZE ICE      2
## 225                  gradient wind      2
## 230                GROUND BLIZZARD      2
## 241                    Gusty winds      2
## 254                       HAIL 150      2
## 261                        HAIL 80      2
## 264                    HAIL DAMAGE      2
## 270                     HAIL/WINDS      2
## 280                     HEAT WAVES      2
## 286            Heavy Precipitation      2
## 295            HEAVY RAIN/FLOODING      2
## 299      HEAVY RAIN/SEVERE WEATHER      2
## 307                     HEAVY SEAS      2
## 308                   HEAVY SHOWER      2
## 314      HEAVY SNOW AND HIGH WINDS      2
## 315             HEAVY SNOW AND ICE      2
## 316       HEAVY SNOW AND ICE STORM      2
## 325       HEAVY SNOW/FREEZING RAIN      2
## 332           HEAVY SNOW/ICE STORM      2
## 334             HEAVY SNOW/SQUALLS      2
## 355                     HIGH TIDES      2
## 358                      High Wind      2
## 360                HIGH WIND (G40)      2
## 365       HIGH WIND AND HIGH TIDES      2
## 367               HIGH WIND DAMAGE      2
## 380                  HIGH WINDS 63      2
## 381                  HIGH WINDS 66      2
## 385                  HIGH WINDS 80      2
## 397                    Hot and Dry      2
## 399                      HOT SPELL      2
## 403              Hurricane Edouard      2
## 406                HURRICANE FELIX      2
## 412                       HVY RAIN      2
## 419                      ICE FLOES      2
## 420                        Ice Fog      2
## 430                       Ice/Snow      2
## 437               Lake Effect Snow      2
## 447                      LANDSPOUT      2
## 452           Late Season Snowfall      2
## 453                      LATE SNOW      2
## 459           LIGHT SNOW AND SLEET      2
## 487                  MARINE MISHAP      2
## 507               Monthly Rainfall      2
## 525                  NON TSTM WIND      2
## 528                           NONE      2
## 552              RECORD COLD/FROST      2
## 555                 RECORD DRYNESS      2
## 559                    Record High      2
## 564            RECORD LOW RAINFALL      2
## 573            Record Temperatures      2
## 582              RED FLAG CRITERIA      2
## 583               RED FLAG FIRE WX      2
## 584              REMNANTS OF FLOYD      2
## 588        RIP CURRENTS/HEAVY SURF      2
## 589         RIVER AND STREAM FLOOD      2
## 593                     ROCK SLIDE      2
## 598                    RURAL FLOOD      2
## 599                   Saharan Dust      2
## 600                   SAHARAN DUST      2
## 611            SLEET/FREEZING RAIN      2
## 614                     SLEET/SNOW      2
## 620   SMALL STREAM AND URBAN FLOOD      2
## 626                 Sml Stream Fld      2
## 633                  SNOW AND COLD      2
## 634            SNOW AND HEAVY SNOW      2
## 653                      SNOW/COLD      2
## 656                SNOW/HIGH WINDS      2
## 677               STRONG WIND GUST      2
## 681              Summary August 10      2
## 682              Summary August 11      2
## 695            Summary of April 12      2
## 697            Summary of April 21      2
## 710             Summary of June 13      2
## 716              Summary of June 3      2
## 721            Summary of March 23      2
## 739           Summary September 23      2
## 742               Summary: Nov. 16      2
## 748              THUDERSTORM WINDS      2
## 749            THUNDEERSTORM WINDS      2
## 755            THUNDERSTORM DAMAGE      2
## 762           THUNDERSTORM WIND 50      2
## 775          THUNDERSTORM WIND G52      2
## 777          THUNDERSTORM WIND G60      2
## 797         THUNDERSTORM WINDS AND      2
## 798 THUNDERSTORM WINDS FUNNEL CLOU      2
## 799           THUNDERSTORM WINDS G      2
## 807      THUNDERSTORM WINDS/ FLOOD      2
## 826             THUNDERSTROM WINDS      2
## 829              THUNDESTORM WINDS      2
## 839                     TORNADO F3      2
## 841                      TORNADOES      2
## 850            TROPICAL STORM DEAN      2
## 855                      Tstm Wind      2
## 865                   TSTM WIND 51      2
## 883        UNSEASONABLY COOL & WET      2
## 889         UNSEASONABLY WARM YEAR      2
## 890          UNSEASONABLY WARM/WET      2
## 892            UNSEASONAL LOW TEMP      2
## 893                UNSEASONAL RAIN      2
## 895          UNUSUAL/RECORD WARMTH      2
## 899                URBAN AND SMALL      2
## 911       URBAN SMALL STREAM FLOOD      2
## 912                    URBAN/SMALL      2
## 915      URBAN/SMALL STREAM  FLOOD      2
## 922                       VERY DRY      2
## 929              VOLCANIC ERUPTION      2
## 930                  WAKE LOW WIND      2
## 940             WATERSPOUT-TORNADO      2
## 942            WATERSPOUT/ TORNADO      2
## 952                      Whirlwind      2
## 981                    WINTERY MIX      2
## 1               HIGH SURF ADVISORY      1
## 2                    COASTAL FLOOD      1
## 3                      FLASH FLOOD      1
## 4                        LIGHTNING      1
## 6                  TSTM WIND (G45)      1
## 7                       WATERSPOUT      1
## 8                             WIND      1
## 9                                ?      1
## 12                  ABNORMALLY WET      1
## 15                   APACHE COUNTY      1
## 18                        AVALANCE      1
## 20                    BEACH EROSIN      1
## 21                   Beach Erosion      1
## 23     BEACH EROSION/COASTAL FLOOD      1
## 26               BITTER WIND CHILL      1
## 32         BLIZZARD AND HEAVY SNOW      1
## 33                Blizzard Summary      1
## 34                BLIZZARD WEATHER      1
## 35          BLIZZARD/FREEZING RAIN      1
## 37              BLIZZARD/HIGH WIND      1
## 38           BLIZZARD/WINTER STORM      1
## 39                   BLOW-OUT TIDE      1
## 40                  BLOW-OUT TIDES      1
## 46  BLOWING SNOW- EXTREME WIND CHI      1
## 47  BLOWING SNOW/EXTREME WIND CHIL      1
## 48                BREAKUP FLOODING      1
## 50                     BRUSH FIRES      1
## 51       COASTAL  FLOODING/EROSION      1
## 52                 COASTAL EROSION      1
## 63                    COASTALFLOOD      1
## 64                    COASTALSTORM      1
## 69                COLD AIR TORNADO      1
## 71                  COLD AND FROST      1
## 72                   COLD AND SNOW      1
## 73         COLD AND WET CONDITIONS      1
## 80                      COLD/WINDS      1
## 81                    COOL AND WET      1
## 82                      COOL SPELL      1
## 85                     DAM FAILURE      1
## 88                       DEEP HAIL      1
## 93                    DRIEST MONTH      1
## 94                   Drifting Snow      1
## 97                        DROWNING      1
## 100                DRY HOT WEATHER      1
## 102              DRY MICROBURST 50      1
## 103              DRY MICROBURST 53      1
## 105              DRY MICROBURST 61      1
## 106              DRY MICROBURST 84      1
## 108           DRY MIRCOBURST WINDS      1
## 109                    DRY PATTERN      1
## 112                        DRYNESS      1
## 113                     DUST DEVEL      1
## 116          DUST DEVIL WATERSPOUT      1
## 118          DUST STORM/HIGH WINDS      1
## 119                      DUSTSTORM      1
## 120                   EARLY FREEZE      1
## 121                    Early Frost      1
## 122                    EARLY FROST      1
## 123                     EARLY RAIN      1
## 128                      EXCESSIVE      1
## 131         EXCESSIVE HEAT/DROUGHT      1
## 132        EXCESSIVE PRECIPITATION      1
## 136              EXCESSIVE WETNESS      1
## 137                EXCESSIVELY DRY      1
## 138                  Extended Cold      1
## 144 EXTREME WIND CHILL/BLOWING SNO      1
## 145            EXTREME WIND CHILLS      1
## 149                  EXTREMELY WET      1
## 151                    FIRST FROST      1
## 156         FLASH FLOOD LANDSLIDES      1
## 157              FLASH FLOOD WINDS      1
## 158                   FLASH FLOOD/      1
## 160            FLASH FLOOD/ STREET      1
## 162         FLASH FLOOD/HEAVY RAIN      1
## 163          FLASH FLOOD/LANDSLIDE      1
## 166 FLASH FLOODING/THUNDERSTORM WI      1
## 168                FLASH FLOOODING      1
## 169                          Flood      1
## 173              FLOOD FLOOD/FLASH      1
## 174                   FLOOD WATCH/      1
## 176              Flood/Flash Flood      1
## 179              FLOOD/FLASH/FLOOD      1
## 180               FLOOD/FLASHFLOOD      1
## 181                FLOOD/RAIN/WIND      1
## 183              FLOOD/RIVER FLOOD      1
## 184              Flood/Strong Wind      1
## 186            FLOODING/HEAVY RAIN      1
## 189      FOG AND COLD TEMPERATURES      1
## 190                   FOREST FIRES      1
## 193               Freezing drizzle      1
## 196  FREEZING DRIZZLE AND FREEZING      1
## 197                   Freezing Fog      1
## 203         FREEZING RAIN AND SNOW      1
## 204        FREEZING RAIN SLEET AND      1
## 205  FREEZING RAIN SLEET AND LIGHT      1
## 208                 Freezing Spray      1
## 211                   Frost/Freeze      1
## 213                  FROST\\FREEZE      1
## 217                  FUNNEL CLOUD.      1
## 218              FUNNEL CLOUD/HAIL      1
## 220                        FUNNELS      1
## 224                GLAZE/ICE STORM      1
## 229                    GRASS FIRES      1
## 232                   GUSTNADO AND      1
## 233                GUSTY LAKE WIND      1
## 236                     Gusty Wind      1
## 238                GUSTY WIND/HAIL      1
## 239            GUSTY WIND/HVY RAIN      1
## 240                Gusty wind/rain      1
## 246                      HAIL 0.88      1
## 247                       HAIL 075      1
## 248                       HAIL 088      1
## 251                     HAIL 1.75)      1
## 253                       HAIL 125      1
## 256                       HAIL 200      1
## 257                       HAIL 225      1
## 259                       HAIL 450      1
## 262                        HAIL 88      1
## 263                     HAIL ALOFT      1
## 265                  HAIL FLOODING      1
## 266                     HAIL STORM      1
## 267                     Hail(0.75)      1
## 268                 HAIL/ICY ROADS      1
## 272                     HAILSTORMS      1
## 274                 HAZARDOUS SURF      1
## 276                   HEAT DROUGHT      1
## 277                      Heat Wave      1
## 279              HEAT WAVE DROUGHT      1
## 281                   HEAT/DROUGHT      1
## 282                      Heatburst      1
## 285            HEAVY PRECIPATATION      1
## 287            HEAVY PRECIPITATION      1
## 291           HEAVY RAIN AND FLOOD      1
## 293             HEAVY RAIN EFFECTS      1
## 294 HEAVY RAIN; URBAN FLOOD WINDS;      1
## 296           Heavy Rain/High Surf      1
## 297           HEAVY RAIN/LIGHTNING      1
## 298     HEAVY RAIN/MUDSLIDES/FLOOD      1
## 300  HEAVY RAIN/SMALL STREAM URBAN      1
## 301                HEAVY RAIN/SNOW      1
## 302         HEAVY RAIN/URBAN FLOOD      1
## 309                  HEAVY SHOWERS      1
## 311     HEAVY SNOW   FREEZING RAIN      1
## 312               HEAVY SNOW & ICE      1
## 313                 HEAVY SNOW AND      1
## 317    HEAVY SNOW AND STRONG WINDS      1
## 318     HEAVY SNOW ANDBLOWING SNOW      1
## 319              Heavy snow shower      1
## 323  HEAVY SNOW/BLIZZARD/AVALANCHE      1
## 324        HEAVY SNOW/BLOWING SNOW      1
## 326                HEAVY SNOW/HIGH      1
## 327           HEAVY SNOW/HIGH WIND      1
## 328          HEAVY SNOW/HIGH WINDS      1
## 329  HEAVY SNOW/HIGH WINDS & FLOOD      1
## 330 HEAVY SNOW/HIGH WINDS/FREEZING      1
## 333               HEAVY SNOW/SLEET      1
## 335                HEAVY SNOW/WIND      1
## 336        HEAVY SNOW/WINTER STORM      1
## 337                 HEAVY SNOWPACK      1
## 340            Heavy surf and wind      1
## 341    HEAVY SURF COASTAL FLOODING      1
## 343                   HEAVY SWELLS      1
## 344                 HEAVY WET SNOW      1
## 345                           HIGH      1
## 346                   HIGH  SWELLS      1
## 347                    HIGH  WINDS      1
## 351           HIGH SURF ADVISORIES      1
## 361                   HIGH WIND 48      1
## 362                   HIGH WIND 63      1
## 363                   HIGH WIND 70      1
## 364       HIGH WIND AND HEAVY SNOW      1
## 366             HIGH WIND AND SEAS      1
## 368            HIGH WIND/ BLIZZARD      1
## 370 HIGH WIND/BLIZZARD/FREEZING RA      1
## 372       HIGH WIND/LOW WIND CHILL      1
## 373                 HIGH WIND/SEAS      1
## 374           HIGH WIND/WIND CHILL      1
## 375  HIGH WIND/WIND CHILL/BLIZZARD      1
## 377                  HIGH WINDS 55      1
## 378                  HIGH WINDS 57      1
## 379                  HIGH WINDS 58      1
## 382                  HIGH WINDS 67      1
## 383                  HIGH WINDS 73      1
## 384                  HIGH WINDS 76      1
## 386                  HIGH WINDS 82      1
## 387      HIGH WINDS AND WIND CHILL      1
## 388          HIGH WINDS DUST STORM      1
## 389         HIGH WINDS HEAVY RAINS      1
## 390                    HIGH WINDS/      1
## 391       HIGH WINDS/COASTAL FLOOD      1
## 393            HIGH WINDS/FLOODING      1
## 394          HIGH WINDS/HEAVY RAIN      1
## 396               HIGHWAY FLOODING      1
## 398                    HOT PATTERN      1
## 400                    HOT WEATHER      1
## 401                HOT/DRY PATTERN      1
## 404                HURRICANE EMILY      1
## 407               HURRICANE GORDON      1
## 409      HURRICANE OPAL/HIGH WINDS      1
## 413          HYPERTHERMIA/EXPOSURE      1
## 414                    HYPOTHERMIA      1
## 418                   ICE AND SNOW      1
## 422           Ice jam flood (minor      1
## 424                    ICE ON ROAD      1
## 425                    ICE PELLETS      1
## 426                      ICE ROADS      1
## 428             ICE STORM AND SNOW      1
## 429          ICE STORM/FLASH FLOOD      1
## 432               ICE/STRONG WINDS      1
## 433              Icestorm/Blizzard      1
## 436                   LACK OF SNOW      1
## 439                     LAKE FLOOD      1
## 443          LANDSLIDE/URBAN FLOOD      1
## 445                      Landslump      1
## 446                      LANDSLUMP      1
## 448               LARGE WALL CLOUD      1
## 449                    LATE FREEZE      1
## 450               LATE SEASON HAIL      1
## 451               LATE SEASON SNOW      1
## 454           Late-season Snowfall      1
## 456                     Light snow      1
## 461     LIGHT SNOW/FREEZING PRECIP      1
## 462                 Light Snowfall      1
## 465             LIGHTNING  WAUSEON      1
## 466       LIGHTNING AND HEAVY RAIN      1
## 467 LIGHTNING AND THUNDERSTORM WIN      1
## 468            LIGHTNING AND WINDS      1
## 469               LIGHTNING DAMAGE      1
## 470                 LIGHTNING FIRE      1
## 471               LIGHTNING INJURY      1
## 472   LIGHTNING THUNDERSTORM WINDS      1
## 473  LIGHTNING THUNDERSTORM WINDSS      1
## 474                     LIGHTNING.      1
## 475           LIGHTNING/HEAVY RAIN      1
## 476                      LIGNTNING      1
## 477              LOCAL FLASH FLOOD      1
## 478                    LOCAL FLOOD      1
## 479             LOCALLY HEAVY RAIN      1
## 481         LOW TEMPERATURE RECORD      1
## 482                 LOW WIND CHILL      1
## 484                Marine Accident      1
## 491            Metro Storm, May 26      1
## 495           Mild and Dry Pattern      1
## 496                   MILD PATTERN      1
## 497               MILD/DRY PATTERN      1
## 498                    MINOR FLOOD      1
## 499                 Minor Flooding      1
## 504                  MODERATE SNOW      1
## 509               Monthly Snowfall      1
## 510               MONTHLY SNOWFALL      1
## 512                 Mountain Snows      1
## 514                     MUD SLIDES      1
## 515      MUD SLIDES URBAN FLOODING      1
## 516                 MUD/ROCK SLIDE      1
## 519             MUDSLIDE/LANDSLIDE      1
## 522               NEAR RECORD SNOW      1
## 523              No Severe Weather      1
## 526         NON-SEVERE WIND DAMAGE      1
## 527                  NON-TSTM WIND      1
## 530                NORTHERN LIGHTS      1
## 534                     PATCHY ICE      1
## 537              PROLONG COLD/SNOW      1
## 541                   RAIN (HEAVY)      1
## 542                  RAIN AND WIND      1
## 543                    Rain Damage      1
## 545                      RAIN/WIND      1
## 546                      RAINSTORM      1
## 547           RAPIDLY RISING WATER      1
## 548                   RECORD  COLD      1
## 551      RECORD COLD AND HIGH WIND      1
## 554               Record dry month      1
## 556                    Record Heat      1
## 558               RECORD HEAT WAVE      1
## 562       RECORD HIGH TEMPERATURES      1
## 565                Record May Snow      1
## 566           RECORD PRECIPITATION      1
## 569               RECORD SNOW/COLD      1
## 575                    RECORD WARM      1
## 576             RECORD WARM TEMPS.      1
## 581      RECORD/EXCESSIVE RAINFALL      1
## 587        RIP CURRENTS HEAVY SURF      1
## 594                     ROGUE WAVE      1
## 601              Seasonal Snowfall      1
## 603                    SEVERE COLD      1
## 607              SEVERE TURBULENCE      1
## 609          SLEET & FREEZING RAIN      1
## 612                SLEET/ICE STORM      1
## 613                SLEET/RAIN/SNOW      1
## 616                     Small Hail      1
## 618                   SMALL STREAM      1
## 619               SMALL STREAM AND      1
## 621 SMALL STREAM AND URBAN FLOODIN      1
## 624       SMALL STREAM URBAN FLOOD      1
## 630              Snow Accumulation      1
## 631              SNOW ACCUMULATION      1
## 632                  SNOW ADVISORY      1
## 635                   Snow and Ice      1
## 637             SNOW AND ICE STORM      1
## 638                 Snow and sleet      1
## 640                  SNOW AND WIND      1
## 644                     SNOW SLEET      1
## 646                   Snow squalls      1
## 649    SNOW- HIGH WIND- WIND CHILL      1
## 650              SNOW/ BITTER COLD      1
## 651                      SNOW/ ICE      1
## 655                SNOW/HEAVY SNOW      1
## 659                      SNOW/RAIN      1
## 660                SNOW/RAIN/SLEET      1
## 663                SNOW/SLEET/RAIN      1
## 664                     SNOW\\COLD      1
## 665                SNOWFALL RECORD      1
## 667                      SNOWSTORM      1
## 668                      SOUTHEAST      1
## 669              STORM FORCE WINDS      1
## 672                STREAM FLOODING      1
## 678                   Strong winds      1
## 683              Summary August 17      1
## 684             Summary August 2-3      1
## 685              Summary August 21      1
## 686              Summary August 28      1
## 687               Summary August 4      1
## 688               Summary August 7      1
## 689               Summary August 9      1
## 690                 Summary Jan 17      1
## 691             Summary July 23-24      1
## 692             Summary June 18-19      1
## 693               Summary June 5-6      1
## 694                 Summary June 6      1
## 696            Summary of April 13      1
## 698            Summary of April 27      1
## 699           Summary of April 3rd      1
## 700            Summary of August 1      1
## 701             Summary of July 11      1
## 702              Summary of July 2      1
## 703             Summary of July 22      1
## 704             Summary of July 26      1
## 705             Summary of July 29      1
## 706              Summary of July 3      1
## 707             Summary of June 10      1
## 708             Summary of June 11      1
## 709             Summary of June 12      1
## 711             Summary of June 15      1
## 712             Summary of June 16      1
## 713             Summary of June 18      1
## 714             Summary of June 23      1
## 715             Summary of June 24      1
## 717             Summary of June 30      1
## 718              Summary of June 4      1
## 719              Summary of June 6      1
## 720            Summary of March 14      1
## 722            Summary of March 24      1
## 723         SUMMARY OF MARCH 24-25      1
## 724            SUMMARY OF MARCH 27      1
## 725            SUMMARY OF MARCH 29      1
## 726              Summary of May 10      1
## 727              Summary of May 13      1
## 728              Summary of May 14      1
## 729              Summary of May 22      1
## 730           Summary of May 22 am      1
## 731           Summary of May 22 pm      1
## 732           Summary of May 26 am      1
## 733           Summary of May 26 pm      1
## 734           Summary of May 31 am      1
## 735           Summary of May 31 pm      1
## 736            Summary of May 9-10      1
## 737            Summary Sept. 25-26      1
## 738           Summary September 20      1
## 740            Summary September 3      1
## 741            Summary September 4      1
## 743              Summary: Nov. 6-7      1
## 744            Summary: Oct. 20-21      1
## 745            Summary: October 31      1
## 746              Summary: Sept. 18      1
## 750            THUNDERESTORM WINDS      1
## 751                    THUNDERSNOW      1
## 752             Thundersnow shower      1
## 756         THUNDERSTORM DAMAGE TO      1
## 757              THUNDERSTORM HAIL      1
## 758            THUNDERSTORM W INDS      1
## 759              Thunderstorm Wind      1
## 761        THUNDERSTORM WIND (G40)      1
## 763           THUNDERSTORM WIND 52      1
## 764           THUNDERSTORM WIND 56      1
## 765           THUNDERSTORM WIND 59      1
## 766       THUNDERSTORM WIND 59 MPH      1
## 767      THUNDERSTORM WIND 59 MPH.      1
## 769       THUNDERSTORM WIND 65 MPH      1
## 770        THUNDERSTORM WIND 65MPH      1
## 771           THUNDERSTORM WIND 69      1
## 772       THUNDERSTORM WIND 98 MPH      1
## 774          THUNDERSTORM WIND G51      1
## 776          THUNDERSTORM WIND G55      1
## 778          THUNDERSTORM WIND G61      1
## 779        THUNDERSTORM WIND TREES      1
## 780             THUNDERSTORM WIND.      1
## 781        THUNDERSTORM WIND/ TREE      1
## 783       THUNDERSTORM WIND/AWNING      1
## 784         THUNDERSTORM WIND/HAIL      1
## 785    THUNDERSTORM WIND/LIGHTNING      1
## 787 THUNDERSTORM WINDS      LE CEN      1
## 788          THUNDERSTORM WINDS 13      1
## 789           THUNDERSTORM WINDS 2      1
## 790          THUNDERSTORM WINDS 50      1
## 791          THUNDERSTORM WINDS 52      1
## 792          THUNDERSTORM WINDS 53      1
## 793          THUNDERSTORM WINDS 60      1
## 794          THUNDERSTORM WINDS 61      1
## 795          THUNDERSTORM WINDS 62      1
## 796      THUNDERSTORM WINDS 63 MPH      1
## 800         THUNDERSTORM WINDS G60      1
## 802  THUNDERSTORM WINDS HEAVY RAIN      1
## 804 THUNDERSTORM WINDS SMALL STREA      1
## 805 THUNDERSTORM WINDS URBAN FLOOD      1
## 808       THUNDERSTORM WINDS/ HAIL      1
## 809 THUNDERSTORM WINDS/FLASH FLOOD      1
## 810    THUNDERSTORM WINDS/FLOODING      1
## 811 THUNDERSTORM WINDS/FUNNEL CLOU      1
## 813  THUNDERSTORM WINDS/HEAVY RAIN      1
## 814           THUNDERSTORM WINDS53      1
## 815         THUNDERSTORM WINDSHAIL      1
## 817              THUNDERSTORM WINS      1
## 821                  THUNDERSTORMW      1
## 822               THUNDERSTORMW 50      1
## 824              THUNDERSTORMWINDS      1
## 825              THUNDERSTROM WIND      1
## 828              THUNDERTSORM WIND      1
## 830              THUNERSTORM WINDS      1
## 831                    TIDAL FLOOD      1
## 835                 TORNADO DEBRIS      1
## 840             TORNADO/WATERSPOUT      1
## 842     TORNADOES, TSTM WIND, HAIL      1
## 843                       TORNADOS      1
## 844                        TORNDAO      1
## 845                TORRENTIAL RAIN      1
## 846            Torrential Rainfall      1
## 849         TROPICAL STORM ALBERTO      1
## 851          TROPICAL STORM GORDON      1
## 853                           TSTM      1
## 857               TSTM WIND  (G45)      1
## 858                 TSTM WIND (41)      1
## 859                TSTM WIND (G35)      1
## 862                   TSTM WIND 40      1
## 863                   TSTM WIND 45      1
## 864                   TSTM WIND 50      1
## 868                  TSTM WIND 65)      1
## 869        TSTM WIND AND LIGHTNING      1
## 870               TSTM WIND DAMAGE      1
## 871                  TSTM WIND G45      1
## 872                  TSTM WIND G58      1
## 875                       TSTM WND      1
## 876                          TSTMW      1
## 878               TUNDERSTORM WIND      1
## 880              Unseasonable Cold      1
## 887        UNSEASONABLY WARM & WET      1
## 897            UNUSUALLY LATE SNOW      1
## 903                    Urban flood      1
## 904                    Urban Flood      1
## 906          URBAN FLOOD LANDSLIDE      1
## 907                 Urban Flooding      1
## 910                    URBAN SMALL      1
## 913           URBAN/SMALL FLOODING      1
## 918          URBAN/SMALL STRM FLDG      1
## 920          URBAN/SML STREAM FLDG      1
## 923                      VERY WARM      1
## 924                            VOG      1
## 925                   Volcanic Ash      1
## 927             Volcanic Ash Plume      1
## 932        WALL CLOUD/FUNNEL CLOUD      1
## 933            WARM DRY CONDITIONS      1
## 934                   WARM WEATHER      1
## 935                    WATER SPOUT      1
## 937        WATERSPOUT FUNNEL CLOUD      1
## 938             WATERSPOUT TORNADO      1
## 941                    WATERSPOUT/      1
## 945                    WAYTERSPOUT      1
## 946                  wet micoburst      1
## 949                       WET SNOW      1
## 950                    WET WEATHER      1
## 953                      WHIRLWIND      1
## 956              WILD/FOREST FIRES      1
## 962                  WIND AND WAVE      1
## 964           WIND CHILL/HIGH WIND      1
## 968                     WIND STORM      1
## 969                      WIND/HAIL      1
## 973        WINTER STORM HIGH WINDS      1
## 974         WINTER STORM/HIGH WIND      1
## 975        WINTER STORM/HIGH WINDS      1
## 983                     Wintry Mix      1
## 985                            WND      1
```

```r
storm$cleanPROPDMG <- NA
storm$cleanCROPDMG <- NA
```


```r
makeCleanEventType <- function(df) {
    df$cleanEventType <- factor(df$EVTYPE)
    levels(df$cleanEventType) <- list(
        "Astronomical Low Tide" = c("ASTRONOMICAL LOW TIDE"),
        "Avalanche" = c("AVALANCE","AVALANCHE","HEAVY SNOW/BLIZZARD/AVALANCHE"),
        "Blizzard" = c("BLIZZARD","BLIZZARD/WINTER STORM","HEAVY SNOW/BLIZZARD","GROUND BLIZZARD","HIGH WIND/BLIZZARD"),
        "Coastal Flood" = c("COASTAL FLOOD","Coastal Flooding","COASTAL FLOODING","COASTAL FLOODING/EROSION", "COASTAL STORM", "COASTALSTORM","Coastal Storm","COASTAL  FLOODING/EROSION","Erosion/Cstl Flood","HEAVY SURF COASTAL FLOODING","HIGH WINDS/COASTAL FLOOD"),
        "Cold/Wind Chill" = c("Cold","COLD","COLD AND SNOW","Cold Temperature","COLD WAVE","COLD WEATHER","COLD/WIND CHILL","COLD/WINDS","Extended Cold","COLD AND WET CONDITIONS","COOL AND WET"),
        "Debris Flow" = c("Debris Flow","LANDSLIDE","LANDSLIDES","Mudslide","Mudslides","Beach Erosion","BLOWING DUST","COASTAL EROSION","Landslump","MUD SLIDE","MUD SLIDES","MUD SLIDES URBAN FLOODING","MUDSLIDE","MUDSLIDES","ROCK SLIDE"),
        "Dense Fog" = c("DENSE FOG"),
        "Dense Smoke" = c("DENSE SMOKE"),
        "Drought" = c("DROUGHT/EXCESSIVE HEAT","DROUGHT"),
        "Dust Devil" = c("DUST DEVIL","Dust Devil","DUST DEVIL WATERSPOUT"),
        "Dust Storm" = c("DUST STORM","DUST STORM/HIGH WINDS"),
        "Excessive Heat" = c("EXCESSIVE HEAT","EXTREME HEAT","RECORD HEAT","RECORD/EXCESSIVE HEAT","UNSEASONABLY WARM","UNSEASONABLY WARM AND DRY"),
        "Extreme Cold/Wind Chill" = c("Extreme Cold","EXTREME COLD","EXTREME COLD/WIND CHILL","HYPERTHERMIA/EXPOSURE","HYPOTHERMIA","Hypothermia/Exposure","HYPOTHERMIA/EXPOSURE","LOW TEMPERATURE","RECORD COLD","UNSEASONABLY COLD","EXTREME WINDCHILL","HIGH WINDS/COLD","Unseasonable Cold","EXTREME WIND CHILL"),
        "Flash Flood" = c(" FLASH FLOOD","FLASH FLOOD","FLASH FLOOD/FLOOD","FLASH FLOODING","FLASH FLOODING/FLOOD","FLASH FLOODS","FLASH FLOOD - HEAVY RAIN","FLASH FLOOD FROM ICE JAMS","FLASH FLOOD LANDSLIDES","FLASH FLOOD WINDS","FLASH FLOOD/","FLASH FLOOD/ STREET","FLASH FLOOD/LANDSLIDE","FLASH FLOODING/THUNDERSTORM WI","RAPIDLY RISING WATER","FLASH FLOOD","ICE STORM/FLASH FLOOD"),
        "Flood" = c("FLOOD","FLOOD & HEAVY RAIN","FLOOD/FLASH FLOOD","FLOOD/RIVER FLOOD","FLOODING","FLOOD FLASH","FLOOD/FLASH","FLOOD/FLASH/FLOOD","FLOOD/FLASHFLOOD","FLOODING/HEAVY RAIN","FLOODS","MINOR FLOODING","RIVER FLOOD","RIVER FLOODING","URBAN AND SMALL STREAM FLOODIN","URBAN/SML STREAM FLD","River Flooding","TIDAL FLOODING","FLOOD/RAIN/WINDS","ICE JAM FLOODING","SMALL STREAM FLOOD","URBAN FLOOD","URBAN FLOODING","BREAKUP FLOODING","HEAVY SNOW/HIGH WINDS & FLOOD","Ice jam flood (minor","MAJOR FLOOD","RIVER AND STREAM FLOOD","RURAL FLOOD","SNOWMELT FLOODING","Tidal Flooding","URBAN FLOODS","URBAN/SMALL STREAM FLOOD","DAM BREAK","ICE JAM",
                    "URBAN AND SMALL","URBAN SMALL","URBAN/SMALL STREAM"),
        "Frost/Freeze" = c("FROST","FREEZE","AGRICULTURAL FREEZE","Damaging Freeze","DAMAGING FREEZE","Early Frost","Freeze","FROST/FREEZE","HARD FREEZE","FROST\\FREEZE"),
        "Funnel Cloud" = c("FUNNEL CLOUD"),
        "Freezing Fog" = c("FOG","FOG AND COLD TEMPERATURES","FREEZING FOG"),
        "Hail" = c("HAIL","HAIL 0.75","HAIL 100","HAIL 175","HAIL 275","HAIL 450","HAIL 75","HAIL DAMAGE","HAIL/WIND","HAIL/WINDS","HAILSTORM","SMALL HAIL","HAIL 075","HAIL 125","HAIL 150","HAIL 200","THUNDERSTORM HAIL","GUSTY WIND/HAIL","WIND/HAIL"),
        "Heat" = c("HEAT","HEAT WAVE","Heat Wave","HEAT WAVE DROUGHT","HEAT WAVES","WARM WEATHER"),
        "Heavy Rain" = c("HEAVY RAIN","HEAVY RAINS","HEAVY RAIN AND FLOOD","Heavy Rain/High Surf","HEAVY RAIN/LIGHTNING","HEAVY RAIN/SEVERE WEATHER","HEAVY RAIN/SMALL STREAM URBAN","HEAVY RAINS/FLOODING","HEAVY SHOWER","HEAVY PRECIPITATION","EXCESSIVE RAINFALL","RAIN/WIND","HIGH WINDS HEAVY RAINS","HVY RAIN","RAIN","RAINSTORM","HIGH WINDS/HEAVY RAIN","UNSEASONAL RAIN","RECORD RAINFALL","Torrential Rainfall"),
        "Heavy Snow" = c("blowing snow","BLOWING SNOW","FALLING SNOW/ICE","HEAVY SNOW","HEAVY SNOW AND HIGH WINDS","SNOW","SNOW SQUALL","Snow Squalls","SNOW/ BITTER COLD","EXCESSIVE SNOW","Heavy snow shower","HIGH WIND/HEAVY SNOW","Snow","HEAVY SNOW-SQUALLS","HEAVY SNOW AND STRONG WINDS","HEAVY SNOW SQUALLS","HEAVY SNOW/SQUALLS","HEAVY SNOW/WIND","SNOW AND HEAVY SNOW","SNOW SQUALLS","SNOW/HEAVY SNOW","HEAVY SNOWPACK","RECORD SNOW","SNOW ACCUMULATION","SNOW/BLOWING SNOW","SNOW/COLD"),
        "High Surf" = c("High Surf","HIGH SURF","Heavy Surf","HEAVY SURF","Heavy surf and wind","HEAVY SURF/HIGH SURF","HIGH SWELLS","HEAVY SWELLS","HIGH WATER","HIGH WAVES","ROUGH SURF","HAZARDOUS SURF","   HIGH SURF ADVISORY"),
        "High Wind" = c("HIGH  WINDS","HIGH WIND","HIGH WIND AND SEAS","HIGH WIND/SEAS","HIGH WINDS","HIGH WINDS/SNOW","HIGH WIND 48","GUSTY WIND","GUSTY WINDS","WIND","WIND STORM","WINDS","Gusty winds","Gusty Winds","NON-SEVERE WIND DAMAGE","NON TSTM WIND","WIND DAMAGE","HIGH WIND (G40)","HIGH WIND DAMAGE","HIGH WINDS/","NON-TSTM WIND","STORM FORCE WINDS","Wind","GUSTY WIND/HVY RAIN","Gusty wind/rain","Wind Damage"),
        "Hurricane (Typhoon)" = c("HURRICANE","HURRICANE ERIN","HURRICANE FELIX","HURRICANE OPAL","HURRICANE OPAL/HIGH WINDS","HURRICANE/TYPHOON","HURRICANE-GENERATED SWELLS","Hurricane Edouard","HURRICANE EMILY","HURRICANE GORDON","TYPHOON"),
        "Ice Storm" = c("BLACK ICE","FREEZING DRIZZLE","Freezing Rain","FREEZING RAIN","FREEZING RAIN/SNOW","Freezing Spray","GLAZE","ICE","ICE ON ROAD","ICE STORM","ICY ROADS","SNOW AND ICE","GLAZE/ICE STORM","HEAVY SNOW/ICE","ICE ROADS","GLAZE ICE","Freezing drizzle","Freezing Drizzle","Glaze","HEAVY SNOW/FREEZING RAIN","ICE AND SNOW","ICE/STRONG WINDS","SNOW AND ICE STORM","SNOW FREEZING RAIN","SNOW/ICE","SNOW/ICE STORM","ICE FLOES","LIGHT FREEZING RAIN","SNOW/ ICE","SNOW/FREEZING RAIN"),
        "Lake-Effect Snow" = c("LAKE-EFFECT SNOW","Lake Effect Snow","LAKE EFFECT SNOW","HEAVY LAKE SNOW"),
        "Lakeshore Flood" = c("LAKE FLOOD","LAKESHORE FLOOD"),
        "Lightning" = c("LIGHTNING  WAUSEON","LIGHTNING","LIGHTNING.","LIGHTNING AND THUNDERSTORM WIN","LIGHTNING INJURY","LIGHTING","LIGHTNING AND HEAVY RAIN","LIGHTNING FIRE","LIGHTNING THUNDERSTORM WINDS","LIGHTNING/HEAVY RAIN","LIGNTNING"),
        "Marine Hail" = c("MARINE HAIL"),
        "Marine High Wind" = c("MARINE HIGH WIND","HIGH SEAS"),
        "Marine Strong Wind" = c("MARINE STRONG WIND","HEAVY SEAS"),
        "Marine Thunderstorm Wind" = c("MARINE THUNDERSTORM WIND","MARINE TSTM WIND","ROUGH SEAS"),
        "Rip Current" = c("RIP CURRENT","RIP CURRENTS","RIP CURRENTS/HEAVY SURF"),
        "Seiche" = c("SEICHE"),
        "Sleet" = c("SLEET","MIXED PRECIP","RAIN/SNOW","HEAVY RAIN/SNOW","FREEZING RAIN/SLEET","SLEET/ICE STORM","SNOW/SLEET","SNOW/SLEET/FREEZING RAIN"),
        "Storm Surge/Tide" = c("STORM SURGE/TIDE","STORM SURGE","COASTAL SURGE","HIGH TIDES"),
        "Strong Wind" = c("STRONG WIND","Strong Winds","STRONG WINDS"),
        "Thunderstorm Wind" = c(" TSTM WIND",                   " TSTM WIND (G45)",				"THUDERSTORM WINDS",             
        "THUNDEERSTORM WINDS",            "THUNDERESTORM WINDS",            "THUNDERSTORM  WINDS",                    
        "Thunderstorm Wind",              "THUNDERSTORM WIND",             
        "THUNDERSTORM WIND (G40)",        "THUNDERSTORM WIND 50",           "THUNDERSTORM WIND 52",           "THUNDERSTORM WIND 56",          
        "THUNDERSTORM WIND 59",           "THUNDERSTORM WIND 59 MPH",       "THUNDERSTORM WIND 59 MPH.",      "THUNDERSTORM WIND 60 MPH",      
        "THUNDERSTORM WIND 65 MPH",       "THUNDERSTORM WIND 65MPH",        "THUNDERSTORM WIND 69",           "THUNDERSTORM WIND 98 MPH",      
        "THUNDERSTORM WIND G50",          "THUNDERSTORM WIND G51",          "THUNDERSTORM WIND G52",          "THUNDERSTORM WIND G55",         
        "THUNDERSTORM WIND G60",          "THUNDERSTORM WIND G61",          "THUNDERSTORM WIND TREES",        "THUNDERSTORM WIND.",            
        "THUNDERSTORM WIND/ TREE",        "THUNDERSTORM WIND/ TREES",       "THUNDERSTORM WIND/AWNING",       "THUNDERSTORM WIND/HAIL",        
        "THUNDERSTORM WIND/LIGHTNING",    "THUNDERSTORM WINDS",             "THUNDERSTORM WINDS      LE CEN", "THUNDERSTORM WINDS 13",         
        "THUNDERSTORM WINDS 2",           "THUNDERSTORM WINDS 50",          "THUNDERSTORM WINDS 52",          "THUNDERSTORM WINDS 53",         
        "THUNDERSTORM WINDS 60",          "THUNDERSTORM WINDS 61",          "THUNDERSTORM WINDS 62",          "THUNDERSTORM WINDS 63 MPH",     
        "THUNDERSTORM WINDS AND",         "THUNDERSTORM WINDS FUNNEL CLOU", "THUNDERSTORM WINDS G",           "THUNDERSTORM WINDS G60",        
        "THUNDERSTORM WINDS HAIL",        "THUNDERSTORM WINDS HEAVY RAIN",  "THUNDERSTORM WINDS LIGHTNING",   "THUNDERSTORM WINDS SMALL STREA",
        "THUNDERSTORM WINDS URBAN FLOOD", "THUNDERSTORM WINDS.",            "THUNDERSTORM WINDS/ FLOOD",      "THUNDERSTORM WINDS/ HAIL",      
        "THUNDERSTORM WINDS/FLASH FLOOD", "THUNDERSTORM WINDS/FLOODING",    "THUNDERSTORM WINDS/FUNNEL CLOU", "THUNDERSTORM WINDS/HAIL",       
        "THUNDERSTORM WINDS/HEAVY RAIN",  "THUNDERSTORM WINDS53",           "THUNDERSTORM WINDSHAIL",         "THUNDERSTORM WINDSS",           
        "THUNDERSTORM WINS",              "THUNDERSTORMS WIND",             "THUNDERSTORMS WINDS",           
        "THUNDERSTORMW",                  "THUNDERSTORMW 50",               "THUNDERSTORMW WINDS",            "THUNDERSTORMWINDS",             
        "THUNDERSTROM WIND",              "THUNDERSTROM WINDS",             "THUNDERTORM WINDS",              "THUNDERTSORM WIND",             
        "THUNDESTORM WINDS",              "THUNERSTORM WINDS",                                      
        "TSTM",                           "Tstm Wind",                      "TSTM WIND",                     
        "TSTM WIND  (G45)",               "TSTM WIND (41)",                 "TSTM WIND (G35)",                "TSTM WIND (G40)",               
        "TSTM WIND (G45)",                "TSTM WIND 40",                   "TSTM WIND 45",                   "TSTM WIND 50",                  
        "TSTM WIND 51",                   "TSTM WIND 52",                   "TSTM WIND 55",                   "TSTM WIND 65)",                 
        "TSTM WIND AND LIGHTNING",        "TSTM WIND DAMAGE",               "TSTM WIND G45",                  "TSTM WIND G58",                 
        "TSTM WIND/HAIL",                 "TSTM WINDS",                     "TSTM WND",                       "TSTMW",
        "SEVERE THUNDERSTORM WINDS","TUNDERSTORM WIND","DRY MICROBURST","DRY MICROBURST WINDS","GUSTNADO","SEVERE THUNDERSTORM","SEVERE THUNDERSTORMS","THUNDERSTORMS","THUNDERSTORM DAMAGE TO","THUNDERSTORM",
        "DOWNBURST","Microburst","MICROBURST","MICROBURST WINDS","DRY MIRCOBURST WINDS","WET MICROBURST"),
        "Tornado" = c("TORNADO","TORNADOES, TSTM WIND, HAIL","TORNADO F0","TORNADO F1","TORNADO F2","TORNADO F3","TORNDAO","TORNADOES","Whirlwind","COLD AIR TORNADO","WHIRLWIND","LANDSPOUT"),
        "Tropical Depression" = c("TROPICAL DEPRESSION"),
        "Tropical Storm" = c("TROPICAL STORM","TROPICAL STORM GORDON","TROPICAL STORM ALBERTO","TROPICAL STORM DEAN","TROPICAL STORM JERRY"),
        "Tsunami" = c("TSUNAMI"),
        "Volcanic Ash" = c("VOLCANIC ASH"),
        "Waterspout" = c("WATERSPOUT","WATERSPOUT/TORNADO","WATERSPOUT-","WATERSPOUT-TORNADO","WATERSPOUT TORNADO","WATERSPOUT/ TORNADO","WATERSPOUT/TORNADO"),
        "Wildfire" = c("WILDFIRE","WILD FIRES","WILD/FOREST FIRE","BRUSH FIRE","FOREST FIRES","WILD/FOREST FIRES","WILDFIRES","GRASS FIRES"),
        "Winter Storm" = c("WINTER STORM","WINTER STORM HIGH WINDS","WINTER STORMS","THUNDERSNOW","SNOW/HIGH WINDS","HEAVY SNOW/WINTER STORM"),
        "Winter Weather" = c("WINTER WEATHER","WINTER WEATHER/MIX","WINTRY MIX","LIGHT SNOW","WINTER WEATHER MIX","HEAVY MIX","LATE SEASON SNOW","Light snow","Light Snow","Light Snowfall","Mixed Precipitation","MIXED PRECIPITATION","Wintry Mix")
    )
    df
}
```

Our cleaning mapped all event types, to one of the 10-1065 document types, except for 1,648 observations which still contain 469 messy event types.

To handle this, we can sift the entire data set by only selecting events which contain positive values in the variables FATALITIES, INJURIES, PROPDMG, and CROPDMG.  The first cut reduces our sample size from 902,297 to 285,851; 6,974 fatalities (f1), 17,604 injuries (i1), 239,174 property damage (p1), and 22,099 crop damage (c1).

There are 4 missing values in fatalities, 5 in injuries, 22 in property damage, and 31 in crop damage.  When we look at the event types for each we see the following for
-fatalities
MARINE MISHAP   Marine Accident DROWNING
-injuries
MARINE MISHAP   HIGH            Marine Accident ROGUE WAVE      OTHER
-property damage
# [1] SEVERE TURBULENCE      APACHE COUNTY          ?                      Other                 
# [5] Marine Accident        OTHER                  GRADIENT WIND          gradient wind         
# [9] Gradient wind          WIND AND WAVE          ASTRONOMICAL HIGH TIDE 
-crop damage
# EXCESSIVE WETNESS OTHER

We won't include this data because they are not one of the 48 values from the 10-1065 document and make a note to ask a subject matter expert for guidance.

We'll query the data as before, and additionally exclude the data with missing values in the cleanEventType and our sample sizes are 6,970 fatalities (f2), 17,559 injuries (i2), 239,152 property damage (p2), and 22,068 crop damage (c2) for a total of 285,749.

We check for missing values in the cleanEventType variable for each subset and find none, so all event types in our samples have been mapped to the 48 allowed values.  We'll do some housekeeping and remove the first four subsets f1, i1, p1, and c2.  At this point, we can total fatalies and injuries by event type.


```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:lubridate':
## 
##     intersect, setdiff, union
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
source("makeCleanEventType.R")
storm <- makeCleanEventType(storm)

stillMessyStorm <- subset(storm,is.na(cleanEventType)) # 1648 obs
unique(stillMessyStorm$EVTYPE) # 469 obs
```

```
##   [1] FUNNEL                         WALL CLOUD                    
##   [3] WALL CLOUD/FUNNEL CLOUD        HAIL 1.75)                    
##   [5] BLIZZARD WEATHER               WIND CHILL                    
##   [7] HIGH WIND AND HIGH TIDES       HIGH WIND/BLIZZARD/FREEZING RA
##   [9] HIGH WIND AND HEAVY SNOW       RECORD COLD AND HIGH WIND     
##  [11] RECORD HIGH TEMPERATURE        RECORD HIGH                   
##  [13] HIGH WIND/ BLIZZARD            BLIZZARD/HIGH WIND            
##  [15] HIGH WIND/LOW WIND CHILL       HEAVY SNOW/HIGH               
##  [17] RECORD LOW                     HIGH WINDS AND WIND CHILL     
##  [19] HEAVY SNOW/HIGH WINDS/FREEZING LOW TEMPERATURE RECORD        
##  [21] MARINE MISHAP                  WIND CHILL/HIGH WIND          
##  [23] HIGH WIND/WIND CHILL/BLIZZARD  HIGH WIND/WIND CHILL          
##  [25] HIGH TEMPERATURE RECORD        FLOOD WATCH/                  
##  [27] RECORD HIGH TEMPERATURES       SEVERE TURBULENCE             
##  [29] RECORD SNOWFALL                RECORD WARMTH                 
##  [31] APACHE COUNTY                  HAIL STORM                    
##  [33] FUNNEL CLOUDS                  WINTER STORM/HIGH WIND        
##  [35] WINTER STORM/HIGH WINDS        SNOW AND WIND                 
##  [37] HEAVY PRECIPATATION            URBAN/SMALL                   
##  [39] HIGH                           URBAN/SMALL FLOODING          
##  [41] WATER SPOUT                    HIGH WINDS DUST STORM         
##  [43] LOCAL FLOOD                    DRY MICROBURST 61             
##  [45] URBAN/SMALL STREAM FLOODING    STREAM FLOODING               
##  [47] GUSTNADO AND                   FLOOD/RAIN/WIND               
##  [49] DOWNBURST WINDS                DRY MICROBURST 53             
##  [51] SMALL STREAM URBAN FLOOD       HIGH WINDS 57                 
##  [53] DRY MICROBURST 50              HIGH WINDS 66                 
##  [55] HIGH WINDS 76                  HIGH WINDS 63                 
##  [57] HIGH WINDS 67                  BLIZZARD/HEAVY SNOW           
##  [59] HEAVY SNOW/HIGH WINDS          HIGH WINDS 82                 
##  [61] HIGH WINDS 80                  HIGH WINDS 58                 
##  [63] LIGHTNING THUNDERSTORM WINDSS  DRY MICROBURST 58             
##  [65] HIGH WINDS 73                  HIGH WINDS 55                 
##  [67] LIGHT SNOW AND SLEET           DRY MICROBURST 84             
##  [69] HEAVY RAIN/FLOODING            TORNADOS                      
##  [71] FIRST SNOW                     FREEZING RAIN AND SLEET       
##  [73] UNSEASONABLY DRY               UNSEASONABLY WET              
##  [75] EXTREME/RECORD COLD            RIP CURRENTS HEAVY SURF       
##  [77] SLEET/RAIN/SNOW                NORMAL PRECIPITATION          
##  [79] HIGH WINDS/FLOODING            DRY                           
##  [81] SNOW/RAIN/SLEET                WATERSPOUTS                   
##  [83] WAYTERSPOUT                    URBAN/SMALL STREAM  FLOOD     
##  [85] URBAN AND SMALL STREAM FLOOD   URBAN AND SMALL STREAM        
##  [87] HEAVY SNOW/HIGH WIND           SMALL STREAM FLOODING         
##  [89] EXTREME WIND CHILLS            SMALL STREAM AND URBAN FLOODIN
##  [91] SMALL STREAM/URBAN FLOOD       SEVERE COLD                   
##  [93] EARLY SNOW                     SMALL STREAM AND URBAN FLOOD  
##  [95] SMALL STREAM AND               HAIL 80                       
##  [97] EXCESSIVE WETNESS              GRADIENT WINDS                
##  [99] HEAVY SNOW/BLOWING SNOW        ROTATING WALL CLOUD           
## [101] LARGE WALL CLOUD               COLD AIR FUNNEL               
## [103] COLD AIR FUNNELS               BLOWING SNOW- EXTREME WIND CHI
## [105] SNOW- HIGH WIND- WIND CHILL    STREET FLOOD                  
## [107] FUNNEL CLOUD/HAIL              ICE/SNOW                      
## [109] THUNDERSTORM DAMAGE            HAIL 1.00                     
## [111] SNOWSTORM                      PROLONG COLD                  
## [113] HAIL 1.75                      HEAVY SNOW/SLEET              
## [115] WARM DRY CONDITIONS            HEAVY SNOW/ICE STORM          
## [117] HIGH WIND 63                   HEAVY SNOW AND ICE STORM      
## [119] MINOR FLOOD                    SNOW/RAIN                     
## [121] BLIZZARD/FREEZING RAIN         HEAVY WET SNOW                
## [123] HAIL 225                       BLIZZARD AND HEAVY SNOW       
## [125] HEAVY SNOW AND ICE             ICE STORM AND SNOW            
## [127] HEAVY SNOW ANDBLOWING SNOW     BLIZZARD AND EXTREME WIND CHIL
## [129] LOW WIND CHILL                 BLOWING SNOW & EXTREME WIND CH
## [131] WATERSPOUT/                    FUNNEL CLOUD.                 
## [133] HAIL 0.88                      DEEP HAIL                     
## [135] HAIL 88                        RECORD HEAT WAVE              
## [137] HAIL FLOODING                  HIGH WIND 70                  
## [139] WET SNOW                       LOCAL FLASH FLOOD             
## [141] FLOOD/FLASH FLOODING           TORNADO/WATERSPOUT            
## [143] RAIN AND WIND                  COASTAL/TIDAL FLOOD           
## [145] BELOW NORMAL PRECIPITATION     EXCESSIVE RAIN                
## [147] LIGHTNING DAMAGE               RECORD TEMPERATURES           
## [149] LIGHTNING AND WINDS            OTHER                         
## [151] LACK OF SNOW                   SNOW DROUGHT                  
## [153] TORRENTIAL RAIN                HEAVY SNOW   FREEZING RAIN    
## [155] DAM FAILURE                    HAIL 088                      
## [157] RECORD COLD/FROST              FREEZING RAIN AND SNOW        
## [159] FREEZING RAIN SLEET AND        SOUTHEAST                     
## [161] HEAVY SNOW & ICE               FREEZING DRIZZLE AND FREEZING 
## [163] HAIL/ICY ROADS                 FLASH FLOOD/HEAVY RAIN        
## [165] HEAVY RAIN; URBAN FLOOD WINDS; FLOOD FLOOD/FLASH             
## [167] SNOW SHOWERS                   RECORD SNOW/COLD              
## [169] WET WEATHER                    FREEZING RAIN SLEET AND LIGHT 
## [171] RECORD/EXCESSIVE RAINFALL      TIDAL FLOOD                   
## [173] BEACH EROSIN                   SLEET & FREEZING RAIN         
## [175] HIGHWAY FLOODING               THUNDERSTORM W INDS           
## [177] FLASH FLOOD/ FLOOD             HEAVY RAIN/MUDSLIDES/FLOOD    
## [179] MUD/ROCK SLIDE                 BEACH EROSION/COASTAL FLOOD   
## [181] SNOW SLEET                     DRY HOT WEATHER               
## [183] HAIL ALOFT                     EARLY FREEZE                  
## [185] EXTREME WIND CHILL/BLOWING SNO EARLY FROST                   
## [187] BLOWING SNOW/EXTREME WIND CHIL FLASH FLOOODING               
## [189] BEACH FLOOD                    HAILSTORMS                    
## [191] FUNNELS                        HEAVY RAINFALL                
## [193] HEAT/DROUGHT                   HEAT DROUGHT                  
## [195] NEAR RECORD SNOW               SLEET/SNOW                    
## [197] EXCESSIVE                      SNOW/SLEET/RAIN               
## [199] DUSTSTORM                      ?                             
## [201] SNOW AND COLD                  HOT PATTERN                   
## [203] PROLONG COLD/SNOW              BRUSH FIRES                   
## [205] SNOW\\COLD                     WINTER MIX                    
## [207] EXCESSIVE PRECIPITATION        SNOWFALL RECORD               
## [209] HOT/DRY PATTERN                DRY PATTERN                   
## [211] MILD/DRY PATTERN               MILD PATTERN                  
## [213] HEAVY SHOWERS                  HEAVY SNOW AND                
## [215] WATERSPOUT FUNNEL CLOUD        URBAN SMALL STREAM FLOOD      
## [217] SAHARAN DUST                   URBAN FLOOD LANDSLIDE         
## [219] SMALL STREAM                   HEAVY RAIN/URBAN FLOOD        
## [221] LANDSLIDE/URBAN FLOOD          Other                         
## [223] Record dry month               Temperature record            
## [225] Minor Flooding                 Marine Accident               
## [227] COASTALFLOOD                   Heavy Rain and Wind           
## [229] Light Snow/Flurries            Wet Month                     
## [231] Wet Year                       Hot and Dry                   
## [233] Flood/Flash Flood              Icy Roads                     
## [235] Rain Damage                    STREET FLOODING               
## [237] Record Cold                    Ice Fog                       
## [239] Excessive Cold                 Late-season Snowfall          
## [241] HEAVY RAIN/WIND                Snow squalls                  
## [243] Strong winds                   RECORD WARM TEMPS.            
## [245] Ice/Snow                       Snow Accumulation             
## [247] Drifting Snow                  Heavy rain                    
## [249] LATE SNOW                      Record May Snow               
## [251] Record Winter Snow             Heavy Precipitation           
## [253]  COASTAL FLOOD                 Record temperature            
## [255] Late Season Snowfall           Gusty Wind                    
## [257] small hail                     Black Ice                     
## [259] Gradient wind                  Snow and Ice                  
## [261] Summary Jan 17                 Summary of March 14           
## [263] Summary of March 23            Summary of March 24           
## [265] Summary of April 3rd           Summary of April 12           
## [267] Summary of April 13            Summary of April 21           
## [269] Summary August 11              Summary of April 27           
## [271] Summary of May 9-10            Summary of May 10             
## [273] Summary of May 13              Summary of May 14             
## [275] Summary of May 22 am           Summary of May 22 pm          
## [277] Heatburst                      Summary of May 26 am          
## [279] Summary of May 26 pm           Metro Storm, May 26           
## [281] Summary of May 31 am           Summary of May 31 pm          
## [283] Summary of June 3              Summary of June 4             
## [285] Summary June 5-6               Summary June 6                
## [287] Summary of June 11             Summary of June 12            
## [289] Summary of June 13             Summary of June 15            
## [291] Summary of June 16             Summary June 18-19            
## [293] Summary of June 23             Summary of June 24            
## [295] Summary of June 30             Summary of July 2             
## [297] Summary of July 3              Summary of July 11            
## [299] Summary of July 22             Summary July 23-24            
## [301] Summary of July 26             Summary of July 29            
## [303] Summary of August 1            Summary August 2-3            
## [305] Summary August 7               Summary August 9              
## [307] Summary August 10              Summary August 17             
## [309] Summary August 21              Summary August 28             
## [311] Summary September 4            Summary September 20          
## [313] Summary September 23           Summary Sept. 25-26           
## [315] Summary: Oct. 20-21            Summary: October 31           
## [317] Summary: Nov. 6-7              Summary: Nov. 16              
## [319] wet micoburst                  Hail(0.75)                    
## [321] Urban Flooding                 No Severe Weather             
## [323] Urban flood                    Urban Flood                   
## [325] Summary of May 22              Summary of June 6             
## [327] Summary August 4               Summary of June 10            
## [329] Summary of June 18             Summary September 3           
## [331] Summary: Sept. 18              coastal flooding              
## [333] Small Hail                     Record Temperatures           
## [335] Blowing Snow                   Early snowfall                
## [337] Monthly Snowfall               Record Heat                   
## [339] Seasonal Snowfall              Monthly Rainfall              
## [341] Sml Stream Fld                 MUDSLIDE/LANDSLIDE            
## [343] Saharan Dust                   Volcanic Ash Plume            
## [345] Thundersnow shower             NONE                          
## [347] SLEET/FREEZING RAIN            BLOW-OUT TIDES                
## [349] UNSEASONABLY COOL              TSTM HEAVY RAIN               
## [351] Wintry mix                     Frost                         
## [353] RAIN (HEAVY)                   Record Warmth                 
## [355] Prolong Cold                   Cold and Frost                
## [357] URBAN/SML STREAM FLDG          STRONG WIND GUST              
## [359] LATE FREEZE                    BLOW-OUT TIDE                 
## [361] Record High                    Snow and sleet                
## [363] Freezing rain                  Blizzard Summary              
## [365] SUMMARY OF MARCH 24-25         SUMMARY OF MARCH 27           
## [367] SUMMARY OF MARCH 29            GRADIENT WIND                 
## [369] Icestorm/Blizzard              Flood/Strong Wind             
## [371] gradient wind                  Mountain Snows                
## [373] URBAN/SMALL STRM FLDG          Mild and Dry Pattern          
## [375] COLD AND FROST                 HIGH  SWELLS                  
## [377] DRY SPELL                       LIGHTNING                    
## [379] BEACH EROSION                  EARLY RAIN                    
## [381] PROLONGED RAIN                 WINTERY MIX                   
## [383] HOT SPELL                      UNSEASONABLY HOT              
## [385] DRY WEATHER                    ABNORMAL WARMTH               
## [387] UNUSUAL WARMTH                 WAKE LOW WIND                 
## [389] MONTHLY RAINFALL               COLD TEMPERATURES             
## [391] COLD WIND CHILL TEMPERATURES   MODERATE SNOW                 
## [393] MODERATE SNOWFALL              URBAN/STREET FLOODING         
## [395] UNUSUAL/RECORD WARMTH          BITTER WIND CHILL             
## [397] BITTER WIND CHILL TEMPERATURES UNSEASONABLY WARM YEAR        
## [399] ICE PELLETS                    PATCHY DENSE FOG              
## [401] RECORD COOL                    RECORD WARM                   
## [403] HOT WEATHER                    RECORD TEMPERATURE            
## [405] VOLCANIC ERUPTION              COOL SPELL                    
## [407] WIND ADVISORY                  RED FLAG FIRE WX              
## [409] FIRST FROST                    EXCESSIVELY DRY               
## [411] SNOW AND SLEET                 LIGHT SNOW/FREEZING PRECIP    
## [413] VOG                            MONTHLY PRECIPITATION         
## [415] MONTHLY TEMPERATURE            RECORD DRYNESS                
## [417] EXTREME WINDCHILL TEMPERATURES DRY CONDITIONS                
## [419] REMNANTS OF FLOYD              EARLY SNOWFALL                
## [421] DRIEST MONTH                   RECORD  COLD                  
## [423] LATE SEASON HAIL               DRYNESS                       
## [425] WIND AND WAVE                   WIND                         
## [427] MONTHLY SNOWFALL               RECORD PRECIPITATION          
## [429] UNSEASONABLY WARM/WET          UNSEASONABLY COOL & WET       
## [431] UNUSUALLY WARM                 NON SEVERE HAIL               
## [433] UNUSUALLY COLD                 LANDSLUMP                     
## [435] UNSEASONABLY WARM & WET        LOCALLY HEAVY RAIN            
## [437] WIND GUSTS                     UNSEASONAL LOW TEMP           
## [439] HIGH SURF ADVISORY             GUSTY LAKE WIND               
## [441] ABNORMALLY DRY                 RED FLAG CRITERIA             
## [443] WND                            CSTL FLOODING/EROSION         
## [445] SMOKE                           WATERSPOUT                   
## [447] SNOW ADVISORY                  EXTREMELY WET                 
## [449] UNUSUALLY LATE SNOW            VERY DRY                      
## [451] RECORD LOW RAINFALL            ROGUE WAVE                    
## [453] PROLONG WARMTH                 ACCUMULATED SNOWFALL          
## [455] DUST DEVEL                     GUSTY THUNDERSTORM WINDS      
## [457] PATCHY ICE                     HEAVY RAIN EFFECTS            
## [459] EXCESSIVE HEAT/DROUGHT         NORTHERN LIGHTS               
## [461] ASTRONOMICAL HIGH TIDE         VERY WARM                     
## [463] ABNORMALLY WET                 TORNADO DEBRIS                
## [465] DROWNING                       GUSTY THUNDERSTORM WIND       
## [467] HIGH SURF ADVISORIES           SLEET STORM                   
## [469] VOLCANIC ASHFALL              
## 985 Levels:    HIGH SURF ADVISORY  COASTAL FLOOD ... WND
```

```r
f1 <- subset(storm,FATALITIES>0) # 6974 obs
i1 <- subset(storm,INJURIES>0) # 17604 obs
p1 <- subset(storm,PROPDMG>0) # 239174 obs
c1 <- subset(storm,CROPDMG>0) # 22099 obs

# there are 4 NA in f, 5 NA in i, 22 NA in p, and 31 NA in c

unique(subset(f1,is.na(f1$cleanEvent))) # MARINE MISHAP   Marine Accident DROWNING
```

```
## Source: local data frame [4 x 40]
## 
##   STATE__          BGN_DATE    BGN_TIME TIME_ZONE COUNTY   COUNTYNAME
## 1       2 1/15/1995 0:00:00        1412       AST      0 AKZ024 - 005
## 2       2 1/27/1995 0:00:00        0940       AST      0 AKZ024 - 005
## 3       6 12/2/1996 0:00:00 12:55:00 PM       PST      1       CAZ001
## 4      42  5/2/2002 0:00:00 04:30:00 PM       EST     71       PAZ071
## Variables not shown: STATE (fctr), EVTYPE (fctr), BGN_RANGE (dbl), BGN_AZI
##   (fctr), BGN_LOCATI (fctr), END_DATE (fctr), END_TIME (fctr), COUNTY_END
##   (dbl), COUNTYENDN (lgl), END_RANGE (dbl), END_AZI (fctr), END_LOCATI
##   (fctr), LENGTH (dbl), WIDTH (dbl), F (int), MAG (dbl), FATALITIES (dbl),
##   INJURIES (dbl), PROPDMG (dbl), PROPDMGEXP (fctr), CROPDMG (dbl),
##   CROPDMGEXP (fctr), WFO (fctr), STATEOFFIC (fctr), ZONENAMES (fctr),
##   LATITUDE (dbl), LONGITUDE (dbl), LATITUDE_E (dbl), LONGITUDE_ (dbl),
##   REMARKS (fctr), REFNUM (dbl), cleanPROPDMG (lgl), cleanCROPDMG (lgl),
##   cleanEventType (fctr)
```

```r
unique(subset(i1,is.na(i1$cleanEvent))) # MARINE MISHAP   HIGH            Marine Accident ROGUE WAVE      OTHER
```

```
## Source: local data frame [5 x 40]
## 
##   STATE__           BGN_DATE    BGN_TIME TIME_ZONE COUNTY
## 1       2  1/27/1995 0:00:00        0940       AST      0
## 2       6 11/16/1994 0:00:00        0800       PST      0
## 3       6  12/2/1996 0:00:00 12:55:00 PM       PST      1
## 4      15   1/4/2001 0:00:00 01:15:00 PM       HST      3
## 5      35  7/15/2001 0:00:00 05:10:00 PM       MST     15
## Variables not shown: COUNTYNAME (fctr), STATE (fctr), EVTYPE (fctr),
##   BGN_RANGE (dbl), BGN_AZI (fctr), BGN_LOCATI (fctr), END_DATE (fctr),
##   END_TIME (fctr), COUNTY_END (dbl), COUNTYENDN (lgl), END_RANGE (dbl),
##   END_AZI (fctr), END_LOCATI (fctr), LENGTH (dbl), WIDTH (dbl), F (int),
##   MAG (dbl), FATALITIES (dbl), INJURIES (dbl), PROPDMG (dbl), PROPDMGEXP
##   (fctr), CROPDMG (dbl), CROPDMGEXP (fctr), WFO (fctr), STATEOFFIC (fctr),
##   ZONENAMES (fctr), LATITUDE (dbl), LONGITUDE (dbl), LATITUDE_E (dbl),
##   LONGITUDE_ (dbl), REMARKS (fctr), REFNUM (dbl), cleanPROPDMG (lgl),
##   cleanCROPDMG (lgl), cleanEventType (fctr)
```

```r
unique(subset(p1,is.na(p1$cleanEvent))) 
```

```
## Source: local data frame [22 x 40]
## 
##    STATE__          BGN_DATE    BGN_TIME TIME_ZONE COUNTY
## 1        2 3/31/1993 0:00:00        1300       AST     15
## 2        4 7/30/1994 0:00:00        0000       MST      5
## 3       54  2/9/1994 0:00:00        0600       EST     23
## 4        2  2/3/1996 0:00:00 12:00:00 AM       AST     19
## 5        6 12/2/1996 0:00:00 12:55:00 PM       PST      1
## 6       32  9/2/1997 0:00:00 02:25:00 PM       PST     31
## 7       40 12/9/1997 0:00:00 04:35:00 PM       CST     31
## 8       48 2/20/1997 0:00:00 11:59:00 AM       CST    227
## 9       48 2/20/1997 0:00:00 03:00:00 PM       CST    237
## 10      48 2/20/1997 0:00:00 03:50:00 PM       CST    213
## ..     ...               ...         ...       ...    ...
## Variables not shown: COUNTYNAME (fctr), STATE (fctr), EVTYPE (fctr),
##   BGN_RANGE (dbl), BGN_AZI (fctr), BGN_LOCATI (fctr), END_DATE (fctr),
##   END_TIME (fctr), COUNTY_END (dbl), COUNTYENDN (lgl), END_RANGE (dbl),
##   END_AZI (fctr), END_LOCATI (fctr), LENGTH (dbl), WIDTH (dbl), F (int),
##   MAG (dbl), FATALITIES (dbl), INJURIES (dbl), PROPDMG (dbl), PROPDMGEXP
##   (fctr), CROPDMG (dbl), CROPDMGEXP (fctr), WFO (fctr), STATEOFFIC (fctr),
##   ZONENAMES (fctr), LATITUDE (dbl), LONGITUDE (dbl), LATITUDE_E (dbl),
##   LONGITUDE_ (dbl), REMARKS (fctr), REFNUM (dbl), cleanPROPDMG (lgl),
##   cleanCROPDMG (lgl), cleanEventType (fctr)
```

```r
# [1] SEVERE TURBULENCE      APACHE COUNTY          ?                      Other                 
# [5] Marine Accident        OTHER                  GRADIENT WIND          gradient wind         
# [9] Gradient wind          WIND AND WAVE          ASTRONOMICAL HIGH TIDE 
unique(subset(c1,is.na(c1$cleanEvent))) # EXCESSIVE WETNESS OTHER
```

```
## Source: local data frame [31 x 40]
## 
##    STATE__         BGN_DATE    BGN_TIME TIME_ZONE COUNTY
## 1       19 6/1/1995 0:00:00        0000       CST      0
## 2       39 5/1/1997 0:00:00 12:00:00 AM       EST      5
## 3       39 5/1/1997 0:00:00 12:00:00 AM       EST      7
## 4       39 5/1/1997 0:00:00 12:00:00 AM       EST     33
## 5       39 5/1/1997 0:00:00 12:00:00 AM       EST     35
## 6       39 5/1/1997 0:00:00 12:00:00 AM       EST     43
## 7       39 5/1/1997 0:00:00 12:00:00 AM       EST     51
## 8       39 5/1/1997 0:00:00 12:00:00 AM       EST     55
## 9       39 5/1/1997 0:00:00 12:00:00 AM       EST     63
## 10      39 5/1/1997 0:00:00 12:00:00 AM       EST     69
## ..     ...              ...         ...       ...    ...
## Variables not shown: COUNTYNAME (fctr), STATE (fctr), EVTYPE (fctr),
##   BGN_RANGE (dbl), BGN_AZI (fctr), BGN_LOCATI (fctr), END_DATE (fctr),
##   END_TIME (fctr), COUNTY_END (dbl), COUNTYENDN (lgl), END_RANGE (dbl),
##   END_AZI (fctr), END_LOCATI (fctr), LENGTH (dbl), WIDTH (dbl), F (int),
##   MAG (dbl), FATALITIES (dbl), INJURIES (dbl), PROPDMG (dbl), PROPDMGEXP
##   (fctr), CROPDMG (dbl), CROPDMGEXP (fctr), WFO (fctr), STATEOFFIC (fctr),
##   ZONENAMES (fctr), LATITUDE (dbl), LONGITUDE (dbl), LATITUDE_E (dbl),
##   LONGITUDE_ (dbl), REMARKS (fctr), REFNUM (dbl), cleanPROPDMG (lgl),
##   cleanCROPDMG (lgl), cleanEventType (fctr)
```

```r
f2 <- subset(storm,FATALITIES>0 & !is.na(cleanEventType)) # 6970 obs
i2 <- subset(storm,INJURIES>0 & !is.na(cleanEventType)) # 17559 obs
p2 <- subset(storm,PROPDMG>0 & !is.na(cleanEventType)) # 239152 obs
c2 <- subset(storm,CROPDMG>0 & !is.na(cleanEventType)) # 22068 obs

sum(is.na(f2$cleanEventType))
```

```
## [1] 0
```

```r
sum(is.na(i2$cleanEventType))
```

```
## [1] 0
```

```r
sum(is.na(p2$cleanEventType))
```

```
## [1] 0
```

```r
sum(is.na(c2$cleanEventType))
```

```
## [1] 0
```

```r
rm(f1);rm(i1);rm(p1);rm(c1)

totF <- summarize(group_by(f2,cleanEventType),sum(FATALITIES))
names(totF) <- c("cleanEventType","totalFatalities")
totI <- summarize(group_by(i2,cleanEventType),sum(INJURIES))
names(totI) <- c("cleanEventType","totalInjuries")
```

Property and crop damage data reside in the PROPDMG, PROPDMGEXP, CROPDMG, and CROPDMGEXP variables.  The 10-1065 document tells us that in te PROPDMGEXP and CROPDMGEXP variables, K or k means multiple of 1000, M or m means multiple of 1,000,000, and B or b means multiple of 1,000,000,000.

We find 327 observations with messy property damage data and 15 observations with messy crop damage data.  Summaries show a maximum of 150 for property damage and 60 for crop damage and the values for PROPDMGEXP are + 0   5 6 4 h 2 7 3 H - and the values for CROPDMGEXP is 0.  The list of event types for these messy property damage observations are:
 [1] Flood             High Wind         Storm Surge/Tide  Flash Flood       Lightning         Tornado          
 [7] Thunderstorm Wind Heavy Snow        Hail              Winter Storm      Ice Storm        
and the list of event types for the messy crop damage observations are:
 [1] Hail              Thunderstorm Wind Tornado

We'll exclude these observations from our analysis because they appear to have negligible effects.  Now we can compute the total property and crop damages.

We'll select data where property and crop damage are positive and cleanEventType does not have missing values, i.e. event successfully mapped to an allowed event type specified in document 10-1065.  But we will also include a test for PROPDMGEXP or CROPDMGEXP must be a valid multiplier of K or k, M or m, or B or b.
This gives us p2 with 238,825 obs, which we divide into three subsets: sskp2 with 227,463 obs, ssmp2 with 11,322 obs, and ssbp2 with 40 obs.  In each subset, we compute the cleanPROPDMG variable using the appropriate multiplier; 1 thousand, 1 million, or 1 billion.  Lastly, we rbind sskp2, ssmp2, and ssbp2 into the final p3 subset that contains the cleanPROPDMG data for the 238,825 observations.

The crop damage data will be processed with the same logic as the property damage data and the final c3 subset contains the cleanCROPDMG data for the 22,053 observations.  We can now group by event type and sum the property and crop damage.


```r
messyProp <- subset(p2,!PROPDMGEXP %in% c("K","M","B","k","m","b")) # 327
unique(messyProp$PROPDMGEXP)
```

```
##  [1] + 0   5 6 4 h 2 7 3 H -
## Levels:  - ? + 0 1 2 3 4 5 6 7 8 B h H K m M
```

```r
unique(messyProp$cleanEventType)
```

```
##  [1] Flood             High Wind         Storm Surge/Tide 
##  [4] Flash Flood       Lightning         Tornado          
##  [7] Thunderstorm Wind Heavy Snow        Hail             
## [10] Winter Storm      Ice Storm        
## 48 Levels: Astronomical Low Tide Avalanche Blizzard ... Winter Weather
```

```r
summary(messyProp$PROPDMG)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    0.10    5.00   15.00   25.07   50.00  150.00
```

```r
messyCrop <- subset(c2,!CROPDMGEXP %in% c("K","M","B","k","m","b")) # 15
unique(messyCrop$CROPDMGEXP)
```

```
## [1] 0  
## Levels:  ? 0 2 B k K m M
```

```r
unique(messyCrop$cleanEventType)
```

```
## [1] Hail              Thunderstorm Wind Tornado          
## 48 Levels: Astronomical Low Tide Avalanche Blizzard ... Winter Weather
```

```r
summary(messyCrop$CROPDMG)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    3.00    5.00    5.00   18.07   25.00   60.00
```

```r
p2 <- subset(storm,PROPDMG>0 & !is.na(cleanEventType) & PROPDMGEXP %in% c("K","M","B","k","m","b")) # 238825 obs
sskp2 <- subset(storm,PROPDMG>0 & !is.na(cleanEventType) & PROPDMGEXP=='K'|PROPDMGEXP=='k') # 227463 obs
ssmp2 <- subset(storm,PROPDMG>0 & !is.na(cleanEventType) & PROPDMGEXP=='M'|PROPDMGEXP=='m') # 11322 obs
ssbp2 <- subset(storm,PROPDMG>0 & !is.na(cleanEventType) & PROPDMGEXP=='B'|PROPDMGEXP=='b') # 40 obs
sskp2$cleanPROPDMG <- sskp2$PROPDMG * 1000
ssmp2$cleanPROPDMG <- ssmp2$PROPDMG * 1000000
ssbp2$cleanPROPDMG <- ssbp2$PROPDMG * 1000000000
p3 <- rbind(sskp2, ssmp2, ssbp2)

c2 <- subset(storm,CROPDMG>0 & !is.na(cleanEventType) & CROPDMGEXP %in% c("K","M","B","k","m","b")) # 22053 obs
sskc2 <- subset(storm,CROPDMG>0 & !is.na(cleanEventType) & CROPDMGEXP=='K'|CROPDMGEXP=='k') # 20128 obs
ssmc2 <- subset(storm,CROPDMG>0 & !is.na(cleanEventType) & CROPDMGEXP=='M'|CROPDMGEXP=='m') # 1918 obs
ssbc2 <- subset(storm,CROPDMG>0 & !is.na(cleanEventType) & CROPDMGEXP=='B'|CROPDMGEXP=='b') # 7 obs
sskc2$cleanCROPDMG <- sskc2$CROPDMG * 1000
ssmc2$cleanCROPDMG <- ssmc2$CROPDMG * 1000000
ssbc2$cleanCROPDMG <- ssbc2$CROPDMG * 1000000000
c3 <- rbind(sskc2, ssmc2, ssbc2)

rm(p2);rm(sskp2);rm(ssmp2);rm(ssbp2)
rm(c2);rm(sskc2);rm(ssmc2);rm(ssbc2)

totP <- summarize(group_by(p3,cleanEventType),sum(cleanPROPDMG))
names(totP) <- c("cleanEventType","totalPropertyDamage")

totC <- summarize(group_by(c3,cleanEventType),sum(cleanCROPDMG))
names(totC) <- c("cleanEventType","totalCropDamage")
```

We can combine the totals for fatalities and injuries into a single measure of population health.  Similarly, property and crop damage can be combined into a single measure of economic consequence.


```r
totHealth1 <- totF
names(totHealth1) <- c("cleanEventType","total")
totHealth2 <- totI
names(totHealth2) <- c("cleanEventType","total")
totHealth <- rbind(totHealth1,totHealth2)
totH <- summarize(group_by(totHealth,cleanEventType),sum(total))
names(totH) <- c("cleanEventType","totalHealthDamage")

totEconomic1 <- totP
names(totEconomic1) <- c("cleanEventType","total")
totEconomic2 <- totC
names(totEconomic2) <- c("cleanEventType","total")
totEconomic <- rbind(totEconomic1,totEconomic2)
totE <- summarize(group_by(totEconomic,cleanEventType),sum(total))
names(totE) <- c("cleanEventType","totalEconomicDamage")
```

We can now produce dot plots of health, fatalities, injuries, economic, property damage, and crop damage for each of the 48 event types.  We'll add three colors to each dot so we can easily see which event types are most harmful to population health and well-being.


```r
par(mfrow = c(3,1))
dotchart(totH$totalHealthDamage,labels=totH$cleanEventType,cex=.7,
         main="Health by Cleaned Event Type",
         xlab="Count of Health",color=c("red","blue", "darkgreen"))
dotchart(totF$totalFatalities,labels=totF$cleanEventType,cex=.7,
         main="Fatalities by Cleaned Event Type",
         xlab="Count of Fatalities",color=c("red","blue", "darkgreen"))
dotchart(totI$totalInjuries,labels=totI$cleanEventType,cex=.7,
         main="Injuries by Cleaned Event Type",
         xlab="Count of Injuries",color=c("red","blue", "darkgreen"))
```

![](project2_files/figure-html/makePlot1-1.png) 


```r
par(mfrow = c(3,1))
dotchart(totE$totalEconomicDamage,labels=totE$cleanEventType,cex=.7,
         main="Economic Damage by Cleaned Event Type",
         xlab="Count of Economic Damage",color=c("red","blue", "darkgreen"))
dotchart(totP$totalPropertyDamage,labels=totP$cleanEventType,cex=.7,
         main="Property Damage by Cleaned Event Type",
         xlab="Total Property Damage",color=c("red","blue", "darkgreen"))
dotchart(totC$totalCropDamage,labels=totC$cleanEventType,cex=.7,
         main="Crop Damage by Cleaned Event Type",
         xlab="Total Crop Damage",color=c("red","blue", "darkgreen"))
```

![](project2_files/figure-html/makePlot2-1.png) 

# Results

## References
[Storm Events Database Event Types](http://www.ncdc.noaa.gov/stormevents/details.jsp)

[Storm Data Publication](http://www.ncdc.noaa.gov/IPS/sd/sd.html#DESCRIPTION)

[Severe weather terminology (United States) wiki](https://en.wikipedia.org/wiki/Severe_weather_terminology_(United_States))

[National Weather Service Storm Data Documentation](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf)

[National Climatic Data Center Storm Events FAQ](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2FNCDC%20Storm%20Events-FAQ%20Page.pdf)
