

# Loading Pre-Processed RDS Files ------------------------------------
# Here, we load all the necessary RDS files. They are precalculated and
# exported to save time from rerunning them.

# CENSUS DATA
mexico_census_analyzed <- read_rds("data/mexico_census_analyzed.rds")
chile_census_analyzed <- read_csv("/Users/carinapeng/Projects/PAHO Risk Assessment/santiago_census_coded.csv")

# MOBILITY DATA
mexico_city_mobility <- read_rds("data/mexico_city_mobility.rds")

# WHO POLICY DATA
who_phsm <- read_rds("data/who_phsm.rds")

#x_test <- unique(who_phsm$who_region)
#x <- data.frame((matrix(unlist(x_test), nrow=length(x_test), byrow=T))
#colnames(x) <- "region"

#observeEvent(input$Select_test, {
#updateSelectInput(session,
#"mydropdown",
#label = "Select neighborhood",             
#choices = file01()[1])

#})

# SERVER
server <- function(input, output, session) {
    
    # Dropdown for city selection based on country selection
    output$dropdown_city <- renderUI({
        if (input$dropdown_country == "Mexico") {
            return(
                selectInput(
                    "city_dropdown",
                    "Select city within Mexico",
                    choices = mexico_census_analyzed$comuna
                )
            )
        }
        
        selectInput("city_dropdown",
                    "Select city within Chile",
                    choices = chile_census_analyzed$comuna)
        
    })
    
    output$dropdown_phsm_country <- renderUI(
        selectInput("dropdown_phsm_country","Select country from PHSM", choices= 
                        as.character(unique(who_phsm$country_territory_area)))
    )
    
    phsm_filtered <- reactive({
        req(input$dropdown_phsm_country)
        phsm1 <- who_phsm %>%
            filter(country_territory_area == input$dropdown_phsm_country)
    })
    
    output$dropdown_phsm_city <- renderUI(
        selectInput("dropdown_phsm_city","Select region from PHSM", choices= 
                        as.character(unique(phsm_filtered()$area_covered)))
    )
    
    # Assign census data based on the dropdown selected
    census <- reactive({ 
        if (input$dropdown_country == "Mexico") {
            return(mexico_census_analyzed)
        }
        chile_census_analyzed
    })
    
    municipal <- reactive({
        req(input$city_dropdown)
        census_mun <- census() %>%
            filter(comuna == input$city_dropdown)
        return(census_mun)
    })
    
    #output$selectfile <- renderUI({
        #if (is.null(input$file1)) {
            #return()
        #}
        #list(
            #hr(),
            #selectInput(
                #"Select_test",
                #"Select Census Data",
                #choices = input$file1$name
            #)
        #)
    #})
    
    
    output$selectfile_incidence <- renderUI({
        if (is.null(input$file1)) {
            return()
        }
        list(
            hr(),
            selectInput(
                "incidence_selectfile",
                "Select Incidence Data",
                choices = input$file1$name
            )
        )
    })
    
    output$selectfile_policy <- renderUI({
        if (is.null(input$file1)) {
            return()
        }
        list(
            hr(),
            selectInput(
                "policy_selectfile",
                "Select WHO PHSM Data",
                choices = input$file1$name
            )
        )
    })
    
    incidence <- reactive({
        req(input$file1)
        read.csv(
            file = input$file1$datapath[input$file1$name == input$incidence_selectfile],
            header = input$header,
            sep = input$sep,
            quote = input$quote
        )
    })
    
    policy <- reactive({
        req(input$file1)
        read.csv(
            file = input$file1$datapath[input$file1$name == input$policy_selectfile],
            header = input$header,
            sep = input$sep,
            quote = input$quote
        )
    })
    
    phsm_municipal <- reactive({
        #req(input$phsm_country)
        #req(input$phsm_area)
        policy_national <- who_phsm() %>%
            filter(country_territory_area == input$phsm_country) %>%
            filter(admin_level == "national")
        policy_area <- policy() %>%
            filter(area_covered == input$phsm_area)
        policy_join <-
            rbind(policy_national, policy_area)
        
        social = ifelse("Social and physical distancing measures" %in% x$who_category,
                        0,
                        1)
        isolation = ifelse("Surveillance and response measures" %in% x$who_category,
                           0,
                           1)
        support = ifelse("Social and physical distancing measures" %in% x$who_category,
                         0,
                         1)
        contact = ifelse("Tracing and quarantining contacts" %in% x$who_subcategory,
                         0,
                         1)
        gathering = ifelse("Gatherings, businesses and services" %in% x$who_subcategory,
                           0,
                           1)
        disturbance = ifelse("Gatherings, businesses and services" %in% x$who_subcategory,
                             0,
                             1)
        
        combined <-
            data.frame(value = rbind(
                social,
                isolation,
                support,
                contact,
                gathering,
                disturbance
            ))
        combined_transpose <- data.frame(transpose(combined))
        rownames(combined_transpose) <- colnames(combined)
        colnames(combined_transpose) <- rownames(combined)
        
        return(combined_transpose)
    })
    
    
    # TEST - Show the dataframe for testing
    output$df_test <- renderTable({
    return(phsm_municipal())
    })
    
    df <- reactive({
        req(input$file1)
        x = incidence()
        x[, 1] <- as.Date(x[, 1], "%d/%m/%Y")
        dfR <-
            estimate_R(x, method = "parametric_si", config = make_config(list(
                mean_si = 4.8, std_si = 2.3
            )))
        return(dfR)
    })
    
    
    #output$plot1 <- renderPlot({
    #plot(df(), what=c("incid"))
    #})
    #output$plot2 <- renderPlot({
    #plot(df(), what=c("R"))
    #})
    
    pop_dens_coded <- reactive({
        req(input$pop_dens)
        if (input$pop_dens >= 0.3 & input$pop_dens <= 0.9) {
            return(0)
        } else if (input$pop_dens >= 0.91 & input$pop_dens <= 2.45) {
            return(1)
        } else if (input$pop_dens >= 2.46 & input$pop_dens <= 8.08) {
            return(2)
        } else if (input$pop_dens >= 8.09 & input$pop_dens <= 22.5) {
            return(3)
        } else if (input$pop_dens >= 22.51) {
            return(4)
        }
    })
    
    #output$testing <- renderText(contexto01())
    
    contexto01 <- reactive({
        if (is.null(municipal()$contexto01)) {
            return(pop_dens_coded())
        }
        else {
            return(as.character(municipal()$contexto01))
        }
    })
    
    water_coded <- reactive({
        req(input$water)
        if (input$water > 75) {
            return(0)
        } else if (input$water >= 50 & input$water <= 75) {
            return(1)
        } else if (input$water < 50) {
            return(2)
        }
    })
    
    contexto02 <- reactive({
        if (is.null(municipal()$contexto02)) {
            return(water_coded())
        }
        else {
            return(as.character(municipal()$contexto02))
        }
    })
    
    occupation_coded <- reactive({
        req(input$occupation)
        if (input$occupation <= 10) {
            return(0)
        } else if (input$occupation >= 11 & input$occupation <= 30) {
            return(1)
        } else if (input$occupation >= 31) {
            return(2)
        }
    })
    
    contexto03 <- reactive({
        if (is.null(municipal()$contexto03)) {
            return(occupation_coded())
        }
        else {
            return(as.character(municipal()$contexto03))
        }
    })
    
    workout_coded <- reactive({
        req(input$workout)
        if (input$workout <= 10) {
            return(0)
        } else if (input$workout >= 11 & input$workout <= 30) {
            return(1)
        } else if (input$workout >= 31) {
            return(2)
        }
    })
    
    contexto04 <- reactive({
        if (is.null(municipal()$contexto04)) {
            return(input$workout)
        }
        else {
            return(as.character(municipal()$contexto04))
        }
    })
    
    publictrans_coded <- reactive({
        req(input$publictrans)
        if (input$publictrans < 50) {
            return(0)
        } else if (input$publictrans >= 51 & input$publictrans <= 65) {
            return(1)
        } else if (input$publictrans >= 65) {
            return(2)
        }
    })
    
    contexto05 <- reactive({
        if (is.null(municipal()$contexto05)) {
            return(publictrans_coded())
        }
        else {
            return(as.character(municipal()$contexto05))
        }
    })
    
    contexto06 <- reactive({
        if (is.null(municipal()$contexto06)) {
            return(input$comorbidity)
        }
        else {
            return(as.character(municipal()$contexto06))
        }
    })
    
    contexto07 <- reactive({
        if (is.null(municipal()$contexto07)) {
            return(input$vac_children)
        }
        else {
            return(as.character(municipal()$contexto07))
        }
    })
    
    contexto08 <- reactive({
        if (is.null(municipal()$contexto08)) {
            return(input$vac_elder)
        }
        else {
            return(as.character(municipal()$contexto08))
        }
    })
    
    stunted_coded <- reactive({
        req(input$stunted)
        if (input$stunted < 30) {
            return(0)
        } else if (input$stunted >= 31) {
            return(1)
        }
    })
    
    contexto09 <- reactive({
        if (is.null(municipal()$contexto09)) {
            return(stunted_coded())
        }
        else {
            return(as.character(municipal()$contexto09))
        }
    })
    
    contexto10 <- reactive({
        if (is.null(municipal()$contexto10)) {
            return(input$employment)
        }
        else {
            return(as.character(municipal()$contexto10))
        }
    })
    
    contexto11 <- reactive({
        if (is.null(municipal()$contexto11)) {
            return(input$income)
        }
        else {
            return(as.character(municipal()$contexto11))
        }
    })
    
    contexto12 <- reactive({
        if (is.null(municipal()$contexto12)) {
            return(input$education)
        }
        else {
            return(as.character(municipal()$contexto12))
        }
    })
    
    contexto13 <- reactive({
        if (is.null(municipal()$contexto13)) {
            return(input$edad65)
        }
        else {
            return(as.character(municipal()$contexto13))
        }
    })
    
    contexto14 <- reactive({
        if (is.null(municipal()$contexto14)) {
            return(input$edad17)
        }
        else {
            return(as.character(municipal()$contexto14))
        }
    })
    
    contexto15 <- reactive({
        if (is.null(municipal()$contexto15)) {
            return(input$disability)
        }
        else {
            return(as.character(municipal()$contexto15))
        }
    })
    
    contexto16 <- reactive({
        if (is.null(municipal()$contexto16)) {
            return(input$single_house)
        }
        else {
            return(as.character(municipal()$contexto16))
        }
    })
    
    contexto17 <- reactive({
        if (is.null(municipal()$contexto17)) {
            return(input$ethnic)
        }
        else {
            return(as.character(municipal()$contexto17))
        }
    })
    
    contexto18 <- reactive({
        if (is.null(municipal()$contexto18)) {
            return(input$multihouse)
        }
        else {
            return(as.character(municipal()$contexto18))
        }
    })
    
    contexto19 <- reactive({
        if (is.null(municipal()$contexto19)) {
            return(input$nbpersons)
        }
        else {
            return(as.character(municipal()$contexto19))
        }
    })
    
    contexto20 <- reactive({
        if (is.null(municipal()$contexto20)) {
            return(input$vehicle)
        }
        else {
            return(as.character(municipal()$contexto20))
        }
    })
    
    contexto21 <- reactive({
        if (is.null(municipal()$contexto21)) {
            return(input$groupq)
        }
        else {
            return(as.character(municipal()$contexto21))
        }
    })
    
    
    output$pop_dens01 <- renderPrint({
        if (is.null(municipal()$contexto01)) {
            #return(writeLines(c("Value NOT found", contexto01())))
            return(writeLines("Please enter value."))
        }
        
        else {
            #return(list(writeLines(c(contexto01())), disable("pop_dens")))
            return(disable("pop_dens"))
        }
    })
    
    output$water02 <- renderPrint({
        if (is.null(municipal()$contexto02)) {
            return(writeLines("Please enter value."))
        }
        
        else {
            return(disable("water"))
        }
    })
    
    output$occupation03 <- renderPrint({
        if (is.null(municipal()$contexto03)) {
            return(writeLines("Please enter value."))
        }
        
        else {
            #return(writeLines(c(contexto03())))
            return(disable("occupation"))
        }
    })
    
    output$workout04 <- renderPrint({
        if (is.null(municipal()$contexto04)) {
            return(writeLines("Please enter value."))
        }
        
        else {
            #return(writeLines(c(contexto04())))
            return(disable("workout"))
        }
    })
    
    output$publictrans05 <- renderPrint({
        if (is.null(municipal()$contexto05)) {
            return(writeLines("Please enter value."))
        }
        
        else {
            #return(writeLines(c(contexto05())))
            return(disable("publictrans"))
        }
    })
    
    output$comorbidity06 <- renderPrint({
        if (is.null(municipal()$contexto06)) {
            return(writeLines("Please enter value."))
        }
        
        else {
            #return(writeLines(c(contexto06())))
            return(disable("comorbidity"))
        }
    })
    
    output$vac_children07 <- renderPrint({
        if (is.null(municipal()$contexto07)) {
            return(
                writeLines(
                    "Please enter value: Proportion of children younger than 1 that has vaccination coverage with three doses of DPT vaccine."
                )
            )
        }
        
        else {
            #return(writeLines(c(contexto07())))
            return(disable("vac_children"))
        }
    })
    
    output$vac_elder08 <- renderPrint({
        if (is.null(municipal()$contexto08)) {
            return(
                writeLines(
                    "Please enter value: Proportion of persons age 60 or older that has vaccination coverage with one dose of influenza vaccine in the previous year."
                )
            )
        }
        
        else {
            #return(writeLines(c(contexto08())))
            return(disable("vac_elder"))
        }
    })
    
    output$stunted09 <- renderPrint({
        if (is.null(municipal()$contexto09)) {
            return(writeLines("Please enter value."))
        }
        
        else {
            #return(writeLines(c(contexto09())))
            return(disable("stunted"))
        }
    })
    
    output$employment10 <- renderPrint({
        if (is.null(municipal()$contexto10)) {
            return(writeLines("Please enter value."))
        }
        
        else {
            return(disable("employment"))
        }
    })
    
    output$income11 <- renderPrint({
        if (is.null(municipal()$contexto11)) {
            return(writeLines("Please enter value."))
        }
        
        else {
            return(disable("income"))
        }
    })
    
    output$education12 <- renderPrint({
        if (is.null(municipal()$contexto12)) {
            return(writeLines("Please enter value."))
        }
        
        else {
            return(disable("education"))
        }
    })
    
    output$edad65_13 <- renderPrint({
        if (is.null(municipal()$contexto13)) {
            return(writeLines("Please enter value."))
        }
        
        else {
            return(disable("edad65"))
        }
    })
    
    output$edad17_14 <- renderPrint({
        if (is.null(municipal()$contexto14)) {
            return(writeLines("Please enter value."))
        }
        
        else {
            return(disable("edad17"))
        }
    })
    
    output$disability15 <- renderPrint({
        if (is.null(municipal()$contexto15)) {
            return(writeLines("Please enter value."))
        }
        
        else {
            return(disable("disability"))
        }
    })
    
    output$single_house15 <- renderPrint({
        if (is.null(municipal()$contexto16)) {
            return(writeLines("Please enter value."))
        }
        
        else {
            return(disable("single_house"))
        }
    })
    
    output$ethnic17 <- renderPrint({
        if (is.null(municipal()$contexto17)) {
            return(writeLines("Please enter value."))
        }
        
        else {
            return(disable("ethnic"))
        }
    })
    
    output$multihouse18 <- renderPrint({
        if (is.null(municipal()$contexto18)) {
            return(writeLines("Please enter value."))
        }
        
        else {
            return(disable("multihouse"))
        }
    })
    
    output$nbpersons19 <- renderPrint({
        if (is.null(municipal()$contexto19)) {
            return(writeLines("Please enter value."))
        }
        
        else {
            return(disable("nbpersons"))
        }
    })
    
    output$vehicle20 <- renderPrint({
        if (is.null(municipal()$contexto20)) {
            return(writeLines("Please enter value."))
        }
        
        else {
            return(disable("vehicle"))
        }
    })
    
    output$groupq21 <- renderPrint({
        if (is.null(municipal()$contexto21)) {
            return(writeLines("Please enter value."))
        }
        
        else {
            return(disable("groupq"))
        }
    })
    
    output$social_dist16 <- renderPrint({
        if (phsm_municipal()$social == 0) {
            return(writeLines("Yes"))
        }
        else{
            return(writeLines("No"))
        }
    })
    
    output$isolation17 <- renderPrint({
        if (phsm_municipal()$isolation == 0) {
            return(writeLines("Yes"))
        }
        else{
            return(writeLines("No"))
        }
    })
    
    output$support18 <- renderPrint({
        if (phsm_municipal()$support == 0) {
            return(writeLines("Yes"))
        }
        else{
            return(writeLines("No"))
        }
    })
    
    speed_coded <- reactive({
        req(input$speed)
        if (input$speed >= 80) {
            return(0)
        } else if (input$speed >= 50 & input$speed <= 79) {
            return(1)
        } else if (input$speed <= 50) {
            return(2)
        }
    })
    
    case_qtine_coded <- reactive({
        req(input$case_qtine)
        if (input$case_qtine >= 80) {
            return(0)
        } else if (input$case_qtine >= 50 & input$case_qtine <= 79) {
            return(1)
        } else if (input$case_qtine <= 50) {
            return(2)
        }
    })
    
    output$contact_tracing21 <- renderPrint({
        if (phsm_municipal()$contact == 0) {
            return(writeLines("Yes"))
        }
        else{
            return(writeLines("No"))
        }
    })
    
    case_contact_coded <- reactive({
        req(input$case_contact)
        if (input$case_contact >= 80) {
            return(0)
        } else if (input$case_contact >= 50 &
                   input$case_contact <= 79) {
            return(1)
        } else if (input$case_contact <= 50) {
            return(2)
        }
    })
    
    output$gathering24 <- renderPrint({
        if (phsm_municipal()$gathering == 0) {
            return(writeLines("Yes"))
        }
        else{
            return(writeLines("No"))
        }
    })
    
    output$disturbance25 <- renderPrint({
        if (phsm_municipal()$disturbance == 0) {
            return(writeLines("Yes"))
        }
        else{
            return(writeLines("No"))
        }
    })
    
    prop_adhere_coded <- reactive({
        req(input$prop_adhere)
        if (input$prop_adhere > 75) {
            return(0)
        } else if (input$prop_adhere >= 51 &
                   input$prop_adhere <= 75) {
            return(1)
        } else if (input$prop_adhere < 50) {
            return(2)
        }
    })
    
    individual <- reactive({
        individual_vunerability <- (
            as.numeric(contexto01()) * 2 +
                as.numeric(contexto02()) * 1 +
                as.numeric(contexto03()) * 1 +
                as.numeric(contexto04()) * 1 +
                as.numeric(contexto05()) * 1 +
                as.numeric(contexto06()) * 3 +
                as.numeric(contexto07()) * 2 +
                as.numeric(contexto08()) * 2 +
                as.numeric(contexto09()) * 2
        )
        return(as.numeric(individual_vunerability))
    })
    
    
    output$context <- renderPrint({
        return(writeLines(as.character(individual())))
    })
    
    individual <- reactive({
        individual_vunerability <- (
            as.numeric(contexto01()) * 2 +
                as.numeric(contexto02()) * 1 +
                as.numeric(contexto03()) * 1 +
                as.numeric(contexto04()) * 1 +
                as.numeric(contexto05()) * 1 +
                as.numeric(contexto06()) * 3 +
                as.numeric(contexto07()) * 2 +
                as.numeric(contexto08()) * 2 +
                as.numeric(contexto09()) * 2
        )
        return(individual_vunerability)
    })
    
    social <- reactive({
        social_vunerability <- (
            as.numeric(contexto10()) * 1 +
                as.numeric(contexto11()) * 1 +
                as.numeric(contexto12()) * 1 +
                as.numeric(contexto13()) * 2 +
                as.numeric(contexto14()) * 1 +
                as.numeric(contexto15()) * 1 +
                as.numeric(contexto16()) * 1 +
                as.numeric(contexto17()) * 2 +
                as.numeric(contexto18()) * 2 +
                as.numeric(contexto19()) * 3 +
                as.numeric(contexto20()) * 1 +
                as.numeric(contexto21()) * 3
        )
        return(social_vunerability)
    })
    
    output$context2 <- renderPrint({
        return(writeLines(as.character(social())))
    })
    
    public_health <- reactive({
        public_health_measures <- (
            as.numeric(phsm_municipal()$social) * 3 +
                as.numeric(phsm_municipal()$isolation) *
                3 +
                as.numeric(phsm_municipal()$support) *
                3 +
                as.numeric(speed_coded()) * 3 +
                as.numeric(case_qtine_coded()) *
                3 +
                as.numeric(phsm_municipal()$contact) *
                2 +
                as.numeric(case_contact_coded()) *
                3 +
                as.numeric(input$prop_test) * 1 +
                as.numeric(phsm_municipal()$gathering) *
                2 +
                as.numeric(phsm_municipal()$disturbance) *
                2 +
                as.numeric(prop_adhere_coded()) *
                3
        )
        return(public_health_measures)
    })
    
    output$miti3 <- renderPrint({
        return(writeLines(as.character(public_health())))
    })
    
    Rt_raw <- reactive({
        x <- df()$R$Mean
        return(round(x[length(x)], digits = 2))
    })
    
    
    #multi_return <- function(){
    #my_list <- list(c(disable("Rt"), paste("The current reproductive number (R) is estimated to be", Rt_raw())))
    #return(my_list)
    #}
    
    output$Rt07 <- renderPrint({
        if (is.null(Rt_raw())) {
            return(writeLines("Please enter value."))
        }
        
        else {
            #return(((multi_return())))
            return(cat(Rt_raw()))
        }
    })
    
    output$Rt07_disable <- renderPrint({
        if (is.null(Rt_raw())) {
            return()
        }
        
        else {
            return(disable("Rt"))
        }
    })
    
    #output$contents5 <- renderPrint({
    #return(writeLines(c("The current reproductive number (R) is estimated to be", Rt())))
    #})
    
    epi_transmission <- reactive({
        epi_trans <- (as.numeric(Rt_raw()) * 2)
        return(epi_trans)
    })
    
    epi_mortality <- reactive({
        epi_mortality_input <- (
            as.numeric(input$case_mort) * 3 +
                as.numeric(input$case_men) * 1 +
                as.numeric(input$case_elder) * 1 +
                as.numeric(input$excess_death) * 2
        )
        return(epi_mortality_input)
    })
    
    output$tbl <- renderTable({
        final_df <- data.frame(
            Category = c(
                "Context - Individual vulnerability",
                "Context - Social vulnerability",
                "Epidemiology - Transmission",
                "Epidemiology - Social vulnerability",
                "Healthcare Systems - Transmission",
                "Healthcare Systems - Mortality"
            ),
            Score = c(
                individual(),
                social(),
                epi_transmission(),
                epi_mortality(),
                social(),
                individual()
            )
        )
        
        return(final_df)
    }, striped = TRUE, bordered = TRUE, hover = TRUE)
    
    # FIX CODE AFTER PRESENTATION
    final_vun <- reactive({
        final_vunerability <- (
            as.numeric(individual()) +
                as.numeric(social()) +
                as.numeric(epi_transmission()) +
                as.numeric(epi_mortality()) +
                as.numeric(social()) +
                as.numeric(individual())
        )
        return(final_vunerability)
    })
    
    output$final_vun_output <- renderPrint({
        return(writeLines(as.character(final_vun())))
    })
    
    final_vun_text <- reactive({
        req(final_vun())
        if (final_vun() >= 0 & final_vun() <= 30) {
            return("Minimal")
        } else if (final_vun() >= 31 & final_vun() <= 60) {
            return("Low")
        } else if (final_vun() >= 61 & final_vun() <= 90) {
            return("Moderate Risk")
        } else if (final_vun() >= 91 & final_vun() <= 120) {
            return("High")
        } else if (final_vun() >= 121 & final_vun() <= 144) {
            return("Very High Risk")
        }
    })
    
    output$final_vun_text <- renderPrint({
        return(writeLines(as.character(final_vun_text())))
    })
    
    final_miti_text <- reactive({
        req(public_health())
        if (public_health() >= 0 & public_health() <= 10) {
            return("Very High")
        } else if (public_health() >= 11 & public_health() <= 20) {
            return("High")
        } else if (public_health() >= 21 & public_health() <= 30) {
            return("Moderate")
        } else if (public_health() >= 31 & public_health() <= 40) {
            return("Low")
        } else if (public_health() >= 41 & public_health() <= 51) {
            return("Minimal")
        }
    })
    
    output$final_miti_text <- renderPrint({
        return(writeLines(as.character(final_miti_text())))
    })
    
    output$final_miti <- renderPrint({
        return(writeLines(as.character(public_health())))
    })
    
}