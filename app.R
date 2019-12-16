#
# App Description Here
#

library(shiny)
library(dplyr)
library(tibble)

# Define UI for application 
ui <- navbarPage(title="Classic Warrior Combat Dummy",
    
    # Instructions Page
    tabPanel("Instructions"),
    # Talent Entry
    tabPanel("Talents",
             column(6,
                    tags$h2("Arms"),
                    selectInput("impHS","Improved Heroic Strike",0:3,selected=3),
                    selectInput("impOP","Improved Overpower",0:2,selected=0),
                    selectInput("angerMan","Anger Management",0:1,selected=1),
                    selectInput("impale","Impale",0:2,selected=2),
                    selectInput("twoHandSpec","Two-Handed Specialization",0:5,selected=0)
                    ),
             column(6,
                    tags$h2("Fury"),
                    selectInput("cruelty","Cruelty",0:5,selected=5),
                    selectInput("unbridledWrath","Unbridled Wrath",0:5,selected=5),
                    selectInput("impBS","Improved Battle Shout",0:5,selected=5),
                    selectInput("impDW","Improved Dual Wield",0:5,selected=5),
                    selectInput("flurry","Flurry",0:5,selected=5),
                    selectInput("bloodthrist","Bloodthirst",0:1,selected=1)
                    )
             ),
    # Gear Entry
    tabPanel("Gear",
             column(6,
               tags$h2("Gear Options"),
               radioButtons("weaponType","Weapon Type",
                            choiceNames = list("Dual Wield","Two-Handed"),
                            choiceValues = list("dualWield","twoHand")),
               checkboxInput("hoj","Hand of Justice",value=TRUE),
               numericInput("sheetCrit","Sheet Crit Chance",value=0.0),
               numericInput("sheetAP","Sheet Attack Power",value=0),
               numericInput("gearHit","Hit Chance from Gear",value=6)
             ),
             column(6,
               tags$h2("Weapon Stats"),
               conditionalPanel(
                 condition = "input.weaponType == 'twoHand'",
                 tags$h4("Two-Hand Weapon"),
                 numericInput("twoHSkill","Weapon Skill",value=305,min=300,max=315),
                 numericInput("twoHMinDmg","Minimum Damage",value=149),
                 numericInput("twoHMaxDmg","Maximum Damage",value=225),
                 numericInput("twoHSpeed","Weapon Speed",value=3.7,min=1.0,max=4.0)
               ),
               conditionalPanel(
                 condition = "input.weaponType == 'dualWield'",
                 tags$h4("Main Hand Weapon"),
                 numericInput("MHSkill","Weapon Skill",value=305,min=300,max=315),
                 numericInput("MHMinDmg","Minimum Damage",value=80),
                 numericInput("MHMaxDmg","Maximum Damage",value=150),
                 numericInput("MHSpeed","Weapon Speed",value=2.7,min=1.0,max=4.0),
                 tags$h4("Off-Hand Weapon"),
                 numericInput("OHSkill","Weapon Skill",value=305,min=300,max=315),
                 numericInput("OHMinDmg","Minimum Damage",value=48),
                 numericInput("OHMaxDmg","Maximum Damage",value=90),
                 numericInput("OHSpeed","Weapon Speed",value=1.7,min=1.0,max=4.0)
               ))
             ),
    # Buffs and Debuffs
    tabPanel("Buffs",
             column(6,
                    tags$h2("Player Buffs"),
                    checkboxInput("windfury","Windfury Totem",value=TRUE),
                    checkboxInput("impWindfury","Improved Weapon Totems",value=FALSE),
                    ),
             column(6,
                    tags$h2("Boss Debuffs"),
                    numericInput("baseArmor","Boss Base Armor",value=3731),
                    checkboxInput("sunder","Sunder Armor",value=TRUE),
                    checkboxInput("ff","Faerie Fire",value=TRUE),
                    checkboxInput("cor","Curse of Recklessness",value=TRUE),
                    checkboxInput("annihilator","Annihilator",value=FALSE),
                    htmlOutput("bossFinalArmor")
                    )
             ),
    # Results
    tabPanel("Simulation",
             sidebarPanel(
                 sliderInput("mins",
                             "Minutes of Combat to Simulate:",
                             min = 1,max = 10,value = 5),
                 actionButton("start","Begin Simulation")
             ),
             mainPanel(
               textOutput("overallDPS"),
               textOutput("flurryUptime"),
               textOutput("hsUptime"),
               tableOutput("abilityTable"),
               tags$h4("Main Hand Auto Attacks"),
               tableOutput("mhAutoTable"),
               tags$h4("Off Hand Auto Attacks"),
               tableOutput("ohAutoTable"),
               tags$h4("Bloodthirst"),
               tableOutput("btTable"),
               tags$h4("Full Log"),
               tableOutput("fullTable")
             )
        ),
    # Caveats and Assumptions
    tabPanel("Caveats",
              tags$h1("THIS IS A WORK IN PROGESS. IT HAS A TON OF FLAWS AND IT IS PROBABLY WRONG."),
              tags$h2("Assumptions"),
              tags$ul(
                tags$li("Unbridled Wrath is a flat 40% (not normalized) and triggers only on autoattacks."),
                tags$li("Dodged auto attacks generate rage based on 75% of that damage that would have hit"),
              ),
             tags$h2("Model Quirks"),
             tags$ul(
               tags$li("This only models the normal rotation against a still target, hitting from behind. It does not include execute at all or use any cooldowns like Deathwish. Therefore, this should only be used for comparison and not for estimating real DPS on a particular boss."),
               tags$li("EVERYTHING in this model happens in 0.4s batches. It attempts to correctly order events that happen in between batch ticks."),
               tags$li("The rotation logic is currently fixed (BT if off cooldown and enough rage; WW if off cooldown, enough rage, and at least 1 sec on BT; HS if 2+ secs on BT and WW and 40 rage; HmS if 3+ secs on BT and WW and 40 rage")
             ),
             tags$h2("Missing Features"),
             tags$ul(
               tags$li("Gear - Would need to build out a database and interface"),
               tags$li("2H Fury - Prompts don't currently do anything. This doesn't seem too bad, but requires additional work and a different ability rotation"),
               tags$li("WF Totem - This seems like a nightmare. Putting it off as long as possible"),
               tags$li("Proc Weapons - These have to be coded individually. This also includes Hand of Justice."),
               tags$li("Any way to adjust the rotation logic - this is probably doable.")
             )
            )
)

# Define server logic
server <- function(input, output) {
  combatLog <- reactiveVal()
  combatLog <- eventReactive(input$start, {

      #######################
      # Initialize Simulation
      #######################
      
      hist <- tribble(
        ~time,
        ~batch,
        ~event,
        ~outcome,
        ~damage,
        ~rageAfter,
        ~GCD,
        ~mhSwingTimer,
        ~ohSwingTimer,
        ~hsQueued,
        ~flurryActive,
        ~flurryStacks
      )
      
      rage <- 0
      batch <- 0
      time <- 0.0
      GCD <- 0.0
      bloodthirstCD <- 0.0
      whirlwindCD <- 0.0
      hsQueued <- FALSE
      
      # Talents
      hsCost <- 15-as.integer(input$impHS)
      flurryBonus <- 0.1*as.integer(input$flurry)
      flurryActive <- FALSE
      flurryStacks <- 0
      angerManagement <- as.logical(as.integer(input$angerMan))
      unbridledWrath <- 0.08*as.integer(input$unbridledWrath)
      impaleCritMod <- 0.1*as.integer(input$impale)
      
      # Weapon Data
      mhMinDmg <- input$MHMinDmg
      mhMaxDmg <- input$MHMaxDmg
      mhSpeed <- input$MHSpeed
      if(input$weaponType == "twoHand") {mhSpeedNorm <- 3.3} else {mhSpeedNorm <- 2.4}
      mhSkill <- input$MHSkill # Eventually calculate based on gear/race
      mhSwingTimer <- 0.0
      mhCrusader <- FALSE # Need to create input
      mhCrusaderBuff <- FALSE # Need to actually code procs
      mhCrusaderTimer <- 0.0
      
      ohMinDmg <- input$OHMinDmg
      ohMaxDmg <- input$OHMaxDmg
      ohSpeed <- input$OHSpeed
      ohSkill <- input$OHSkill # Eventually calculate based on gear/race
      ohSwingTimer <- 0.0
      ohCrusader <- FALSE # Need to create input
      ohCrusaderBuff <- FALSE # Need to actually code procs
      ohCrusaderTimer <- 0.0
      
      # Damage Stats
      finalCrit <- (input$sheetCrit/100) - 0.048 - 0.002 # .002 reduction due to nonexistant weaponskill sheet modifier. Remove this eventually
      finalHit <- input$gearHit/100
      baseAP <- input$sheetAP
      finalBossArmor <- max(input$baseArmor-(input$sunder*2250)-(input$ff*505)-(input$cor*640)-(input$annihilator*600),0)
      PDR <- 1-finalBossArmor/(finalBossArmor+400+(85*60))
      hsMod <- 138 # Check for AQ20 R9 book at some point
      
      currAP <- function(){return(baseAP+200*mhCrusader+200*ohCrusader)}
            
      mhHitDmg <- function() {return(round((runif(1,mhMinDmg,mhMaxDmg) + mhSpeed*(currAP()/14))*PDR,0))}
      ohHitDmg <- function() {return(round((runif(1,ohMinDmg,ohMaxDmg) + ohSpeed*(currAP()/14))*0.5*(1+(0.05*as.integer(input$impDW)))*PDR,0))}
      normHitDmg <- function() {return(round((runif(1,mhMinDmg,mhMaxDmg) + mhSpeedNorm*(currAP()/14))*PDR,0))}
      
      # Hit table data and functions
      mhSkillDiff <- 315 - mhSkill
      mhBaseMiss <- ifelse(mhSkillDiff > 10, 0.05 + mhSkillDiff*0.002, 0.05 + mhSkillDiff*0.001) # Also the "yellow" miss chance
      mhBaseDWMiss <- mhBaseMiss*0.8 + 0.2
      mhMissChance <- max(mhBaseMiss - finalHit,0)
      mhDWMissChance <- max(mhBaseDWMiss - finalHit,0)
      mhDodgeChance <- 0.05 + mhSkillDiff*0.001
      mhGlanceChance <- 0.4
      mhGlancePenLow <- min(1.3 - 0.05*mhSkillDiff,0.91)
      mhGlancePenHigh <- min(1.2 - 0.03*mhSkillDiff,0.99)
      #mhAPFactor <- 
      
      ohSkillDiff <- 315 - ohSkill
      ohBaseMiss <- ifelse(ohSkillDiff > 10, 0.05 + ohSkillDiff*0.002, 0.05 + ohSkillDiff*0.001)
      ohBaseDWMiss <- ohBaseMiss*0.8 + 0.2
      ohMissChance <- max(ohBaseMiss - finalHit,0) # To be used when Heroic Strike is queued
      ohDWMissChance <- max(ohBaseDWMiss - finalHit,0)
      ohDodgeChance <- 0.05 + ohSkillDiff*0.001
      ohGlanceChance <- 0.4
      ohGlancePenLow <- min(1.3 - 0.05*ohSkillDiff,0.91)
      ohGlancePenHigh <- min(1.2 - 0.03*ohSkillDiff,0.99)
      #ohAPFactor <- 

      
      swingOutcome <- function(attack){
        hitRoll <- runif(1)
        missChance <- 0
        dodgeChance <- 0 
        glanceChance <- 0
        critChance <- 0
        
        if(attack == "MH Auto"){
          missChance <- mhDWMissChance
          dodgeChance <- mhDodgeChance
          glanceChance <- mhGlanceChance
          critChance <- min(finalCrit,1-sum(missChance,dodgeChance,glanceChance))
        }
        if(attack == "OH Auto"){
          missChance <- ifelse(hsQueued,ohMissChance,ohDWMissChance) # Eliminate DW penalty while HS is queued
          dodgeChance <- ohDodgeChance
          glanceChance <- ohGlanceChance
          critChance <- min(finalCrit,1-sum(missChance,dodgeChance,glanceChance))
        }
        if(attack %in% c("Bloodthirst","Whirlwind","Hamstring","Heroic Strike")){
          missChance <- mhBaseMiss
          dodgeChance <- mhDodgeChance
          glanceChance <- 0
          critChance <- finalCrit
        }
        if(hitRoll < missChance) {return("MISS")} else
          if(hitRoll < missChance + dodgeChance) {return("DODGE")} else
            if(hitRoll < missChance + dodgeChance + glanceChance) {return("GLANCE")} else
              if(attack %in% c("Bloodthirst","Whirlwind","Hamstring","Heroic Strike")) {
                critRoll <- runif(1)
                if(critRoll < critChance){return("CRIT")} else {return("HIT")}
              } else
                if(hitRoll < missChance + dodgeChance + glanceChance + critChance) {return("CRIT")} else
                  return("HIT")
      }
   
      swingDamage <- function(attack,outcome){
        if(outcome=="MISS") {return(0)}
        if(outcome=="DODGE") {
          if(attack=="MH Auto"){
            return(mhHitDmg()*0.75) # Autoattack dodges generate 75% of damage as rage
          }
          if(attack=="OH Auto"){
            return(ohHitDmg()*0.75) # Autoattack dodges generate 75% of damage as rage
          }
          return(0)
        }
        if(outcome=="GLANCE"){
          if(attack=="MH Auto"){
            pen <- runif(1,min=mhGlancePenLow,max=mhGlancePenHigh)
            return(round(mhHitDmg()*pen,0)) 
          }
          if(attack=="OH Auto"){
            pen <- runif(1,min=ohGlancePenLow,max=ohGlancePenHigh)
            return(round(ohHitDmg()*pen,0))
          }
          return(0)
        }
        if(outcome=="CRIT"){
          if(attack=="MH Auto"){return(mhHitDmg()*2)}
          if(attack=="OH Auto"){return(ohHitDmg()*2)}
          if(attack=="Heroic Strike"){return(round((mhHitDmg()+hsMod)*(2+impaleCritMod),0))}
          if(attack=="Bloodthirst"){return(round(currAP()*0.45*(2+impaleCritMod)*PDR,0))}
          if(attack=="Whirlwind"){return(round(normHitDmg()*(2+impaleCritMod),0))}
          if(attack=="Hamstring"){return(round(45*(2+impaleCritMod)*PDR,0))}
        }
        if(outcome=="HIT"){
          if(attack=="MH Auto"){return(mhHitDmg())}
          if(attack=="OH Auto"){return(ohHitDmg())}
          if(attack=="Heroic Strike"){return(round(mhHitDmg()+hsMod,0))}
          if(attack=="Bloodthirst"){return(round(currAP()*0.45*PDR,0))}
          if(attack=="Whirlwind"){return(round(normHitDmg(),0))}
          if(attack=="Hamstring"){return(round(45*PDR,0))}
        }
        return(0) # Failsafe
      }
      
      updateRage <- function(attack,outcome,damage){
        if(attack =="MH Auto") {
          if(outcome %in% c("HIT","GLANCE","DODGE")) {
            rage <- min(rage + min((15*damage)/(4*230.6) + (3.5*mhSpeed/2),(15*damage)/230.6),100)
          }
          if(outcome == "CRIT") {
            rage <- min(rage + min((15*damage)/(4*230.6) + (7*mhSpeed/2),(15*damage)/230.6),100)
          }
        }
        if(attack =="OH Auto") {
          if(outcome %in% c("HIT","GLANCE","DODGE")) {
            rage <- min(rage + min((15*damage)/(4*230.6) + (1.75*ohSpeed/2),(15*damage)/230.6),100)
          }
          if(outcome == "CRIT") {
            rage <- min(rage + min((15*damage)/(4*230.6) + (3.5*ohSpeed/2),(15*damage)/230.6),100)
          }
        }
        if(attack %in% c('MH Auto','OH Auto') & outcome %in% c('HIT','GLANCE','CRIT') & unbridledWrath > 0 & runif(1) < unbridledWrath) {
          rage <- min(rage+1, 100)
        }
        if(attack == "Bloodthirst"){
          if(outcome %in% c('MISS','DODGE')){rage <- rage-6}
          else {rage <- rage-30}
        }
        if(attack == "Whirlwind"){rage <- rage-25}
        if(attack == "Heroic Strike"){
          if(outcome %in% c("MISS","DODGE")){rage <- rage-round(hsCost*0.2,0)}
          else {rage <- rage-hsCost}
          }
        if(attack == "Hamstring"){
          if(outcome %in% c("MISS","DODGE")){rage <- rage-2}
          else {rage <- rage-10}
        }
        return(rage)
      }
      
      logEvent <- function(event, outcome, damage) {
        return(hist %>% add_row(
          time=time,
          batch=batch,
          event=event,
          outcome=outcome,
          damage=damage,
          rageAfter=rage,
          GCD=GCD,
          mhSwingTimer=mhSwingTimer,
          ohSwingTimer=ohSwingTimer,
          hsQueued=hsQueued,
          flurryActive=flurryActive,
          flurryStacks=flurryStacks
        ))
      }
      
      ##################
      # Start Simulation
      ##################

      while(time < input$mins*60){
        # Process Timers
        mhSwingRemain <- mhSpeed - mhSwingTimer
        ohSwingRemain <- ohSpeed - ohSwingTimer
        useGCD <- 1 # Flag to be used to break loop if GCD is up but nothing can be done
        
        # Use abilities in order they came off cooldown between batches
        while (mhSwingRemain <= 0 | ohSwingRemain <= 0 | (GCD <= 0 & useGCD == 1)) {
          #print(paste0("mhSwingRemain: ",mhSwingRemain," ohSwingRemain: ",ohSwingRemain," GCD: ",GCD," useGCD: ",useGCD))
          if (mhSwingRemain <= ohSwingRemain & mhSwingRemain <= GCD) {
            mhSwingTimer <- mhSwingTimer - mhSpeed
            mhSwingRemain <- mhSpeed - mhSwingTimer
            outcome <- "MISS" # Priming for flurry check later
            
            if(hsQueued){
              outcome <- swingOutcome("Heroic Strike")
              damage <- swingDamage("Heroic Strike", outcome)
              rage <- updateRage("Heroic Strike", outcome, damage)
              hist <- logEvent("Heroic Strike", outcome, damage)
              hsQueued <- FALSE
            } else {
              outcome <- swingOutcome("MH Auto")
              damage <- swingDamage("MH Auto", outcome)
              rage <- updateRage("MH Auto", outcome, damage)
              hist <- logEvent("MH Auto", outcome, damage)
            }
            
            if(flurryActive) {
              flurryStacks <- max(flurryStacks-1,0)
              if(flurryStacks == 0) {flurryActive <- FALSE}
            }
            if(outcome == "CRIT" & flurryBonus > 0){
              flurryActive <- TRUE
              flurryStacks <- 3
            }
          }
          if (ohSwingRemain < mhSwingRemain & ohSwingRemain <= GCD) {
            ohSwingTimer <- ohSwingTimer - ohSpeed
            ohSwingRemain <- ohSpeed - ohSwingTimer
            
            outcome <- swingOutcome("OH Auto")
            damage <- swingDamage("OH Auto", outcome)
            rage <- updateRage("OH Auto", outcome, damage)
            hist <- logEvent("OH Auto", outcome, damage)
          
            if(flurryActive) {
              flurryStacks <- max(flurryStacks-1,0)
              if(flurryStacks == 0) {flurryActive <- FALSE}
            }
            if(outcome == "CRIT" & flurryBonus > 0){
              flurryActive <- TRUE
              flurryStacks <- 3
            }
          }
          if (GCD <= mhSwingRemain & GCD <= ohSwingRemain & useGCD == 1) {
            # Rotation Logic to choose ability for GCD
            outcome <- "MISS" # Priming for flurry check later on
            if(bloodthirstCD <= 0 & rage >= 30){
              outcome <- swingOutcome("Bloodthirst")
              damage <- swingDamage("Bloodthirst", outcome)
              rage <- updateRage("Bloodthirst", outcome, damage)
              GCD <- GCD + 1.5
              useGCD <- 0
              hist <- logEvent("Bloodthirst", outcome, damage)
              bloodthirstCD <- bloodthirstCD + 6
            }
            if(whirlwindCD <= 0 & rage >= 25 & bloodthirstCD >= 1){
              outcome <- swingOutcome("Whirlwind")
              damage <- swingDamage("Whirlwind", outcome)
              rage <- updateRage("Whirlwind", outcome, damage)
              GCD <- GCD + 1.5
              useGCD <- 0
              hist <- logEvent("Whirlwind", outcome, damage)
              whirlwindCD <- whirlwindCD + 10
            }
            if(whirlwindCD >= 2 & bloodthirstCD >= 2 & rage >= 40 & hsQueued == FALSE){
              hsQueued <- TRUE
            }
            if(whirlwindCD >= 3 & bloodthirstCD >= 3 & rage >= 40){
              outcome <- swingOutcome("Hamstring")
              damage <- swingDamage("Hamstring", outcome)
              rage <- updateRage("Hamstring", outcome, damage)
              GCD <- GCD + 1.5
              useGCD <- 0
              hist <- logEvent("Hamstring", outcome, damage)
            }
            if(outcome == "CRIT" & flurryBonus > 0){
              flurryActive <- TRUE
              flurryStacks <- 3
            }
            if(useGCD == 1){
              useGCD <- 0
              if(GCD < 0){GCD <- 0}
            }
          } 
        }
        
        # Increment Timers
        batch <- batch + 1
        time <- time + 0.4
        if(GCD > 0){GCD <- GCD - 0.4}
        if(bloodthirstCD > 0) {bloodthirstCD <- bloodthirstCD - 0.4}
        if(whirlwindCD > 0) {whirlwindCD <- whirlwindCD - 0.4}
        mhSwingTimer <- mhSwingTimer + 0.4*(1+flurryBonus*flurryActive)
        ohSwingTimer <- ohSwingTimer + 0.4*(1+flurryBonus*flurryActive)
        if(time%%3 < 0.4) {rage <- rage + 1*angerManagement}
      }
      return(hist)
    },ignoreInit = TRUE)
  
  ################
  # Create Outputs
  ################
  output$bossFinalArmor <- renderUI({
    finalArmor <- max(input$baseArmor-(input$sunder*2250)-(input$ff*505)-(input$cor*640)-(input$annihilator*600),0)
    pdr <- finalArmor/(finalArmor+400+85*60)
    HTML(
      paste0("Boss Effective Armor: ",finalArmor,"<br/>",
             "Physical Damage Reduction: ",round(100*pdr,0),"%")
      )
  })
  output$overallDPS <- renderText({
    paste0("Overall DPS: ",round((combatLog() %>% summarise(sum(damage)/(input$mins*60))),1))
  })
  output$flurryUptime <- renderText({
    paste0("Flurry Uptime: ",round(100 * (combatLog() %>% summarise(sum(flurryActive)/n())),1), "%")
  })
  output$hsUptime <- renderText({
    paste0("Heroic Strike Uptime: ",round(100 * (combatLog() %>% summarise(sum(hsQueued)/n())),1), "%")
  })
    output$abilityTable <- renderTable({
      combatLog() %>%
        group_by(event) %>%
        summarise(
          dps=sum(damage)/(input$mins*60),
          avgDmg=mean(damage), 
          count=n(), 
          critPct=paste0(round(100 * sum(outcome=="CRIT")/n(), 1), "%")
          ) %>%
        mutate(dpsBreakdown=paste0(round(100 * dps/sum(dps), 1), "%"))
    })
    output$mhAutoTable <- renderTable({
      combatLog() %>%
        filter(event == "MH Auto") %>% 
        group_by(outcome) %>%
        summarise(avgDmg=mean(damage), count=n()) %>%
        mutate(freq=paste0(round(100 * count/sum(count), 1), "%"))
    })
    output$ohAutoTable <- renderTable({
      combatLog() %>%
        filter(event == "OH Auto") %>% 
        group_by(outcome) %>%
        summarise(avgDmg=mean(damage), count=n()) %>%
        mutate(freq=paste0(round(100 * count/sum(count), 1), "%"))
    })
    output$btTable <- renderTable({
      combatLog() %>%
        filter(event == "Bloodthirst") %>% 
        group_by(outcome) %>%
        summarise(avgDmg=mean(damage), count=n()) %>%
        mutate(freq=paste0(round(100 * count/sum(count), 1), "%"))
    })
    output$fullTable <- renderTable({
      combatLog()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
