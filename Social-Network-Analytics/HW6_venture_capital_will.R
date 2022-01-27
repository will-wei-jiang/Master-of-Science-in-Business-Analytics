############ Set Up ############
library(tidyverse)
library(data.table)
library(igraph)
library(lubridate)
library(RSiena)
library(survival)
library(gdata)
library(proxy)
library(plm)
library(zoo)
library(stringr)
library(sna)
library(geosphere)
library(nnet)
library(Matrix)

investor <- fread("investor_details.csv", header = TRUE)
deal <- fread("deal_details.csv", header = TRUE)
company <- fread("company_details.csv", header = TRUE)
exec <- fread("execs.csv", header = TRUE)
ind_investor <- fread("individual_investors.csv", header = TRUE)
people <- fread("people.csv", header = TRUE)


############ Basic Analysis Framework ############

# Here in this homework, I will analyze the investor network not at the level of the firm, 
# but at the level of the individual.
# The key idea is that the homophily can contribute to a culture of "failing fast", 
# in which start-up founders can become serial founders as a result of being selected not on their prior performance, 
# but on shared personal or network attributes with investors that might choose to invest in their start-ups

# In the following part I will analyze the individual-level investor-entrepreneur network 
# an the performance of those companies to understand whether this process will actually
# make their companies more successful.

# In order to estimate a regression in which the outcome variable is the network itself,
# I will use SIENA models to estimate funding PersonIds (investors) chooses to invest in funded PersonIds (executives)

# Along with the default rate of network change parameters and parameter for venture capital 
# outdegree that are automatically included in the model, 
# I also include in the model predictors for following types of variables using the includeEffects() function

# The following steps first shows data cleaning and preparing, 
# then I will select a category(retail) as an example, 
# then I will generate year data as the wave in the model,
# then I will generate dyadic predictors based on individual attributes,
# then I will generate individual predictors for entrepreneurs

# Note: I did not finish all the steps mentioned above as time is limited.


############ Data Preparing ############

# only consider investments to be all deals in the Deal Class "Venture Capital".
deal <- filter(deal,Deal_Class == 'Venture Capital')

# transfer deal date to year level
deal$Deal_Date <- year(dmy(deal$Deal_Date))

# to make sure that the nodes in the network are likely to be drawn from a more established community, 
# limit the analysis to only deals from the 2000s onward
deal <- filter(deal,Deal_Date >= 2000)

# isolate the effects of investor-entrepreneur homophily, as opposed to broader effects of founding team diversity
# focus the set of entrepreneurs just on those whose Full Title is indicated to be "Founder", "Chief Executive Officer", or "Managing Director"
exec <- filter(exec, Full.Title %in% c('Chief Executive Officer','Founder','Managing Director'))

# generate full network by combining all information as below

# generate a column in exec regarding the number of executives by company, then split data in order to set unique keys
exec[, exec_num := seq_len(.N), by = CompanyId]
exec_split <- split(exec, f = exec$exec_num)
for(i in seq_along(exec_split)){
  setkey(exec_split[[i]], CompanyId)
}
setkey(ind_investor, CompanyId)

# combine the ind_investor and exec_split data together
network <- list()
for(i in seq_along(exec_split)){
  network[[i]] <- merge(ind_investor, exec_split[[i]])
  print(paste0("executive num ", i))
  flush.console()
}
network <- rbindlist(network)
setkey(network, DealId)

# change the name to distinguish between investor personal ID and executive personal ID
network <- network %>% 
  rename(investor = PersonId.x, exec = PersonId.y)

# combine the deal data to the network table
setkey(deal, DealId)
network <- merge(network, deal[,-2])

# then combine the company information table to the network
colnames(company)[1] <- 'CompanyId' #change to same column name as network
setkey(company, CompanyId)
setkey(network, CompanyId)
network <- merge(network, company[, c("CompanyId", "Primary_Industry_Sector", "Primary_Industry_Group", "Primary_Industry_Code", "Business_Status", "City", "State")])

# then combine the people (investor) information table to the network
people_sub <- people[,c("PersonId", "Gender", "Education")]
colnames(people_sub)[colnames(people_sub) == "PersonId"] = "investor"
setkey(people_sub, investor)
setkey(network, investor)
network <- merge(network, people_sub)

# then combine the people (exec) information table to the network
colnames(people_sub)[colnames(people_sub) == "investor"] = "exec"
setkey(people_sub, exec)
setkey(network, exec)
network <- merge(network, people_sub)

# change the column name to show information more clear
network <- network %>% 
  rename(Gender_investor=Gender.x, Education_investor=Education.x,
         Gender_exec=Gender.y, Education_exec=Education.y)

# in order for the network to be bipartite, 
# exclude individuals that appear as both investors and entrepreneurs in the data
network <- network[!exec %in% investor]
network <- network[!investor %in% exec]


############ Select an Industry Category (Retail) ############

# check how many unique Primary_Industry_Group
unique(network$Primary_Industry_Group)

# check the number of rows of different Primary_Industry_Group
network %>% 
  group_by(Primary_Industry_Group) %>% 
  summarise(cnt=n()) %>% 
  arrange(desc(cnt))

# select a category for the following analysis
# choose criteria: high count but not the top ones + personal interests
retail_network <- network[Primary_Industry_Group=='Retail']


############ Generate Year Data as Wave in the Model ############

# construct wave by year for the model by using igarph as follows

# generate graph from data frame
retail_graph <- graph_from_data_frame(retail_network[, c('investor', 'exec')], directed = TRUE)
V(retail_graph)$type <- V(retail_graph)$name %in% retail_graph$exec

# get the pairwise list of investors and exec
retail_edge <- data.table(ends(retail_graph, seq_len(ecount(retail_graph))))
colnames(retail_edge) <- c('investor', 'exec')
setkeyv(retail_edge, c('investor', 'exec'))

# generate a new column edge position by using increment integers
retail_edge[, edge_position := .I]

# get the earliest and latest year by investor and exec pair
retail_network[, earliest_year := min(Deal_Date, na.rm = TRUE), by = c('investor', 'exec')]
retail_network[, latest_year := max(Deal_Date, na.rm = TRUE), by = c('investor', 'exec')]

# combine the two tables
years <- unique(retail_network[,c('investor', 'exec', 'earliest_year', 'latest_year')])
setkeyv(years, c('investor', 'exec'))
retail_edge <- merge(retail_edge, years, all.x = TRUE)

# deal with network time
YearData <- seq(min(as.numeric(retail_network$Deal_Date), na.rm = TRUE), 
               max(as.numeric(retail_network$Deal_Date), na.rm = TRUE))

investment_year = lapply(seq_along(YearData), 
                        function(i) igraph::delete.edges(retail_graph, retail_edge$edge_position[retail_edge$earliest_year > YearData[i] | YearData[i] - retail_edge$latest_year > 5]))

# create the matrix for each year by using for loop
investment_data <- list()
for (i in 1:19){
temp_n = get.incidence(investment_year[[i]], sparse = FALSE)
investment_data[[i]] <- temp_n
}

# setup the Siena data as an array by using the matrix by year
retail_investment <- sienaDependent(array(c(investment_data[[1]], 
                                           investment_data[[2]],  
                                           investment_data[[3]],  
                                           investment_data[[4]],  
                                           investment_data[[5]],  
                                           investment_data[[6]],  
                                           investment_data[[7]], 
                                           investment_data[[8]],
                                           investment_data[[9]],
                                           investment_data[[10]],
                                           investment_data[[11]],
                                           investment_data[[12]],
                                           investment_data[[13]],
                                           investment_data[[14]],
                                           investment_data[[15]],
                                           investment_data[[16]],
                                           investment_data[[17]],
                                           investment_data[[18]],
                                           investment_data[[19]]), 
                                          c(dim(investment_data[[1]]), length(YearData))),
                                    type = 'bipartite', 
                                    nodeSet = c('Senders', 'Receivers'))

# make sure we set up the node sets using the sienaNodeSet function
Senders <- sienaNodeSet(length(unique(retail_network$investor)), nodeSetName = "Senders")
Receivers <- sienaNodeSet(length(unique(retail_network$exec)), nodeSetName = "Receivers")


############ Generate Dyadic Predictors Based on Individual Attributes ############

# ethnic homophily -------------------------------------------------------------

Ethics <- unique(fread('representative_names.csv', header = TRUE)[,c('Name','race')])

# get the race information of people (investor) and combine them to the network
retail_investor <- merge(unique(retail_network[,'investor']),people[,c('PersonId','Last Name')],by.x='investor',by.y='PersonId')
retail_investor$`Last Name` <- toupper(retail_investor$`Last Name`) 
retail_investor <- merge(retail_investor,Ethics,by.x='Last Name',by.y='Name',all.x = TRUE)
retail_investor[which(retail_investor$race=='white'),'race']<-NA

# get the race information of people (exec) and combine them to the network
retail_exec <- merge(unique(retail_network[,'exec']),people[,c('PersonId','Last Name')],by.x='exec',by.y='PersonId')
retail_exec$`Last Name` <- toupper(retail_exec$`Last Name`) 
retail_exec <- merge(retail_exec,Ethics,by.x='Last Name',by.y='Name',all.x = TRUE)
retail_exec[which(retail_exec$race=='white'),'race']<-NA

# convert data to vector
retail_investor <- as.vector(setNames(retail_investor$race,retail_investor$investor))
retail_exec <- as.vector(setNames(retail_exec$race,retail_exec$exec))

# create the Ethics matrix
cov_matrix <- outer(retail_investor,retail_exec,'==')
cov_matrix[is.na(cov_matrix)] <- 0
Ethics_Matrix <- coDyadCovar(cov_matrix,nodeSets=c('Senders','Receivers'),type='bipartite')

# gender homophily -------------------------------------------------------------

# get the gender information for both investors and exec
gender_investor <- unique(retail_network[,c('investor','Gender_investor')])
gender_exec <- unique(retail_network[,c('exec','Gender_exec')])

# convert data to vector
gender_investor <- as.vector(setNames(gender_investor$Gender_investor,gender_investor$investor))
gender_exec <- as.vector(setNames(gender_exec$Gender_exec,gender_exec$exec))

# create the Gender matrix
cov_matrix <- outer(gender_investor,gender_exec,'==')
cov_matrix[is.na(cov_matrix)] <- 0
Gender_Matrix <- coDyadCovar(cov_matrix,nodeSets=c('Senders','Receivers'),type='bipartite')

# top school homophily ---------------------------------------------------------

edu_investor <- unique(retail_network[,c('investor','Education_investor')])
edu_exec <- unique(retail_network[,c('exec','Education_exec')])

# generate a list of TOP schools (Ivy League + US additional schools + worldwide schools)
TopSchools <- c("Brown University","Columbia University","Cornell University","Dartmouth College","Harvard","Princeton University","University of Pennsylvania","Yale University","MIT","Stanford","Northwestern University","Massachusetts Institute of Technology","Berkeley","Columbia Business School","University of Chicago","Carnegie Mellon University","Oxford","Cambridge")

# check people has educational background from one of the top schools
edu_investor$TopSchools <- grepl(paste(TopSchools,collapse="|"), edu_investor$Education_investor)
edu_exec$TopSchools <- grepl(paste(TopSchools,collapse="|"), edu_exec$Education_exec)

# convert data to vector
edu_investor <- as.vector(setNames(edu_investor$TopSchools,edu_investor$investor))
edu_exec <- as.vector(setNames(edu_exec$TopSchools,edu_exec$exec))

# create the Edu matrix
cov_matrix <- outer(edu_investor,edu_exec,'==')
cov_matrix[is.na(cov_matrix)] <- 0
Edu_Matrix <- coDyadCovar(cov_matrix,nodeSets=c('Senders','Receivers'),type='bipartite')

# geographic homophily ---------------------------------------------------------

load('edges_dist.RData')

geo_investor <- unique(retail_network[,c("investor","InvestorId")])
geo_exec <- unique(retail_network[,c("exec","CompanyId")])

# merge those data with location information
geo_investor <- merge(geo_investor,unique(edges_dist[,c(1,6,5)]),by="InvestorId",all.x=TRUE)
geo_exec <- merge(geo_exec,unique(edges_dist[,c(2,4,3)]),by="CompanyId",all.x=TRUE)

# avoid repeat and only keep first observation for each individual
geo_investor[, no := seq_len(.N), by = investor]
geo_investor <- geo_investor[which(no==1),]
geo_exec[, no := seq_len(.N), by = exec]
geo_exec <- geo_exec[which(no==1),]

# create the Geo matrix
geo_investor <- data.frame(unique(geo_investor[,c(2:4)]), row.names = 1)
geo_exec <- data.frame(unique(geo_exec[,c(2:4)]), row.names = 1)
cov_matrix <- distm(as.matrix(geo_investor),as.matrix(geo_exec),fun=distGeo)
rownames(cov_matrix) <- rownames(geo_investor)
colnames(cov_matrix) <- rownames(geo_exec)
cov_matrix[cov_matrix=="NaN"] <- NA
Geo_Matrix <- coDyadCovar(cov_matrix,nodeSets=c('Senders','Receivers'),type='bipartite')

# experience homophily ---------------------------------------------------------

exp_investor <- unique(retail_network[,c('investor','Deal_Date')])
exp_exec <- unique(retail_network[,c('exec','Deal_Date')])

exp_investor <- exp_investor %>%
  group_by(investor) %>% 
  summarise(first=min(Deal_Date))

exp_exec <- exp_exec %>% 
  group_by(exec) %>% 
  summarise(first=min(Deal_Date))

# convert data to vector
exp_investor <- as.vector(setNames(exp_investor$first,exp_investor$investor))
exp_exec <- as.vector(setNames(exp_exec$first,exp_exec$exec))

# create the Exp matrix
cov_matrix <- outer(exp_investor,exp_exec,'==')
cov_matrix[is.na(cov_matrix)] <- 0
Exp_Matrix <- coDyadCovar(cov_matrix,nodeSets=c('Senders','Receivers'),type='bipartite')

# complementary skills ---------------------------------------------------------

skill_investor <- unique(retail_network[,c('investor','Education_investor')])
skill_exec <- unique(retail_network[,c('exec','Education_exec')])

#We check if each person has technical degree or business degree from the top schools
skill_investor$tech <- grepl("engineering|ph.D", skill_investor$Education_investor)
skill_investor$business <- grepl("MBA", skill_investor$Education_investor)
skill_exec$tech <- grepl("engineering|ph.D", skill_exec$Education_exec)
skill_exec$business <- grepl("MBA", skill_exec$Education_exec)

# convert data to vector
skill_investor_tech <- as.vector(setNames(skill_investor$tech,skill_investor$investor))
skill_exec_tech <- as.vector(setNames(skill_exec$tech,skill_exec$exec))
skill_exec_business <- as.vector(setNames(skill_exec$business,skill_exec$exec))
skill_investor_business <- as.vector(setNames(skill_investor$business,skill_investor$investor))

# create the Skill matrix
cov_matrix1 <- outer(skill_investor_tech,skill_exec_business,"&")
cov_matrix2 <- outer(skill_investor_business,skill_exec_tech,"&")
cov_matrix <- cov_matrix1 + cov_matrix2
rownames(cov_matrix) <- rownames(skill_investor)
colnames(cov_matrix) <- rownames(skill_exec)
Skill_Matrix <- coDyadCovar(cov_matrix,nodeSets=c('Senders','Receivers'),type='bipartite')


############ Generate Individual Predictors for Entrepreneurs ############

# entrepreneur ethnic minority -------------------------------------------------

# entrepreneur gender ----------------------------------------------------------

# entrepreneur top school ------------------------------------------------------

# entrepreneur geographic hub --------------------------------------------------

# entrepreneur experience ------------------------------------------------------

# entrepreneur business skills -------------------------------------------------

# entrepreneur technical skills ------------------------------------------------

# entrepreneur venture round ---------------------------------------------------


############ Runing the SIENA Model ############

# setup for a model using 4 cores would be
siena_result = siena07(siena_algorithm, 
                       data = siena_data, 
                       effects = siena_effects,
                       nbrNodes = 4, 
                       useCluster = TRUE, initC = TRUE)



############ Extra Credit ############

# Q(A) -------------------------------------------------------------------------
# predict whether investing based on homophily will help investors avoid going out of business

# get the clean data
outcomes <- fread("individual_investor_outcomes.csv", header = TRUE)

# run regression
out1 <- glm(out_of_business ~ l4c_scaled +  gender + ethnicity + age_diff + geo_dist + ivyplus + complementarity + male_exec + nonwhite_exec + ivyplus_exec + year + inv_long + inv_lat, data = outcomes, family = "binomial")
summary(out1)

# Comment: from the regression result above, we can find that l4c_scaled, ethnicity, age_diff, ivyplus, male_exec and inv_lat
# is statistically significant with out of business,
# however, some of them, such as l4c_scaled, age_diff and inv_lat are negatively correlated, 
# others, such as ethnicity, male_exec are positively correlated.
# Therefore, investing based on homophily will not help investors avoid going out of business for sure.

# Q(B) -------------------------------------------------------------------------
# predict whether investing based on homophily help entrepreneurs' ventures achieve better outcomes such as Exit or Profitable

# get the clean data
startup_states <- fread("startup_states.csv", header = TRUE)

# run regression
out2 <- multinom(company_state ~ l4c_scaled +  gender + ethnicity + age_diff + geo_dist + ivyplus + complementarity + male_exec + nonwhite_exec + ivyplus_exec + year + comp_lon + comp_lat, data = startup_states, family = "binomial")
z = summary(out2)$coefficients/summary(out2)$standard.errors
# ff the value returned is below 0.05, then we can conclude that the predictor was significant.
print( (1 - pnorm(abs(z), 0, 1)) * 2)

# Comment: the significance test of the z-scores of the coefficients shows that nearly all coefficients expect are significant
# but geo_distance is not statistically significant for predicting companies categorized as profitable or not
# Thus we can draw a basic conclusion that investing based on homophily help entrepreneurs' ventures achieve better outcomes
# For start-ups, failing fast through homophily also influences the trajectory of startups across the board



