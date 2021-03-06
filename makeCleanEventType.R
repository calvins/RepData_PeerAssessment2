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



