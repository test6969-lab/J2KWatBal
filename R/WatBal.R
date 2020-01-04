#' Daily water balance of the basin
#'
#'More info in the basic of results on J2000 hydrological model
#'@param TimeLoop The timeloop file present in output folder of J2K model
#'
#'@return Plot with the daily water balance
#'
#'@examples
#'WatBal_plot() <- plot(x=years, y= daily water storage in basin)
#'
#'@export


WatBal_plot <- function(TimeLoop){
  #read header names
  variable_head <- unlist(strsplit(scan(paste(Output,"TimeLoop.dat",sep=""),"",nlines = 1,skip = 5,sep = "\n"),split = "\t"))
  #read data
  timeloop <- fread(paste(Output,"TimeLoop.dat",sep=""),skip = 10)
  timeloop <- timeloop[,1:length(variable_head)]
  #assign column names
  colnames(timeloop) <- c("Date",variable_head[-1])
  timeloop$Date <- as.Date(timeloop$Date, format= "%Y-%m-%d")
  timeloop$year <- format(as.Date(timeloop$Date), format= "%Y")


  #Calculation for the Daily WaterBalance
  WaterBalance <- timeloop %>%
    arrange(Date) %>%
    mutate(	Year = year,
            Date = Date,
            Input = precip + iceRunoff,
            Output = actET + catchmentRD1_w + catchmentRD2_w + catchmentRG1_w + catchmentRG2_w,
            diffIntStor = intercStorage - lag(intercStorage, default = first(intercStorage)),
            diffSWE = snowTotSWE - lag(snowTotSWE, default = first(snowTotSWE)),
            diffSnowS_G = snowStorage_G - lag(snowStorage_G, default = first(snowStorage_G)),
            diffactMPS = actMPS - lag(actMPS, default = first(actMPS)),
            diffactLPS = actLPS - lag(actLPS, default = first(actLPS)),
            diffactRG1 = actRG1 - lag(actRG1, default = first(actRG1)),
            diffactRG2 = actRG2 - lag(actRG2, default = first(actRG2)),
            diffchannelStorage_w = channelStorage_w - lag(channelStorage_w, default = first(channelStorage_w)),
            diffactDPS = actDPS - lag(actDPS, default = first(actDPS)),
            glacestor = glacStorage,
            Storage = diffIntStor + diffSWE + diffSnowS_G + diffactMPS+ diffactLPS + diffactRG1 + diffactRG2 + diffchannelStorage_w + diffactDPS + glacestor,
            WatBal = Input - Output - Storage
    )
  WaterBalance <- WaterBalance[-1,]

  #Visualization of the timeLoop
  return(ggplot(data=WaterBalance, mapping=aes(y =  WatBal, x = Date))+
           geom_line(color="blue") +
           theme_bw() + ylab("Water Balance"))

}


#'Annual plot of input vs output
#'
#'Visulization of annual input and outputs
#'
#'@param TimeLoop Output from J2K model
#'
#'@return Annual input and output information
#'
#'@examples
#'WatBal_inVSout <- plot(x=years, y=value)
#'
#'@export

WatBal_inVSout <- function(TimeLoop){

  #read header names
  variable_head <- unlist(strsplit(scan(paste(Output,"TimeLoop.dat",sep=""),"",nlines = 1,skip = 5,sep = "\n"),split = "\t"))
  #read data
  timeloop <- fread(paste(Output,"TimeLoop.dat",sep=""),skip = 10)
  timeloop <- timeloop[,1:length(variable_head)]
  #assign column names
  colnames(timeloop) <- c("Date",variable_head[-1])
  timeloop$Date <- as.Date(timeloop$Date, format= "%Y-%m-%d")
  timeloop$year <- format(as.Date(timeloop$Date), format= "%Y")

  #For the analysis of the Input and output variables
  #Removing the years with less than 360 years of data
  timeloop <- timeloop %>%
    add_count(year) %>%
    filter(n > 360)

  #Visulization of the Input and Output variables
  WaterBalance2 <- timeloop %>%
    group_by(year)%>%
    summarise(
      Precip = sum(precip),
      Ice_runoff = sum(iceRunoff),
      actET = sum(actET),
      Annual_RD1 = sum(catchmentRD1_w),
      Annual_RD2 = sum(catchmentRD2_w),
      Annual_RG1 = sum(catchmentRG1_w),
      Annual_RG2 = sum(catchmentRG2_w),
      Snowmelt_Out_G = sum(snowMelt),
      Glacier_runoff = sum(glacierRunoff), #glacierRunoff=snowmeltg+icerunoff+rainrunoff
      SnowMelt_G = sum(snowMelt_G), #plus iceRunoff
      Rain_runoff = sum(rainRunoff)
    )

  a <- which(colnames(WaterBalance2)== "Precip")
  b <- which(colnames(WaterBalance2)== "Ice_runoff")
  c <- which(colnames(WaterBalance2)== "actET")
  d <- which(colnames(WaterBalance2)== "Annual_RD1")
  e <- which(colnames(WaterBalance2)== "Annual_RD2")
  f <- which(colnames(WaterBalance2)== "Annual_RG1")
  g <- which(colnames(WaterBalance2)== "Annual_RG2")
  h <- which(colnames(WaterBalance2)== "Snowmelt_Out_G")

WaterBalance3 <- WaterBalance2 %>%
    mutate(
      Input = rowSums(.[a:b]),
      Total_Discharge = rowSums(.[d:g]),
      Output = rowSums(.[c:g]),
      PERofET_inPrecip = (actET/Precip*100),
      PERofRD1_inQ = (Annual_RD1/Total_Discharge*100),
      PERofRD2_inQ = (Annual_RD2/Total_Discharge*100),
      PERofRG1_inQ = (Annual_RG1/Total_Discharge*100),
      PERofRG2_inQ = (Annual_RG2/Total_Discharge*100),
      PERofIce_inGlaRunoff = (Ice_runoff/Glacier_runoff*100),
      PERofSnowMelt_G_inGlaRunoff = (SnowMelt_G/Glacier_runoff*100),
      PERofRainRunof_inGlaRunoff = (Rain_runoff/Glacier_runoff*100)
    )

#Visualization of the Input Vs Output
w4 <-WaterBalance3 %>%
  select(year,Input,Output)

w4 <- reshape2::melt(w4, id.vars="year")

return(ggplot(w4, aes(variable,value)) +
         geom_col(aes(fill=variable),position="identity")+
         scale_fill_manual(values=c("#56B4E9","#E69F00"), "Legend") +
         theme_classic()+ ylab("mm") + xlab("") +
         facet_grid(.~year))

}


#'Major components of the water balance
#'
#'Visulization of major components groups in Input, Output and Snow/Glacier
#'
#'@param TimeLoop Output from J2K model
#'
#'@return Info on the water balance
#'
#'@examples
#'WatBal_majorcomponents() <- plot(x=varables, y=value)
#'
#'@export


WatBal_majorcomponents <- function(TimeLoop){

  #read header names
  variable_head <- unlist(strsplit(scan(paste(Output,"TimeLoop.dat",sep=""),"",nlines = 1,skip = 5,sep = "\n"),split = "\t"))
  #read data
  timeloop <- fread(paste(Output,"TimeLoop.dat",sep=""),skip = 10)
  timeloop <- timeloop[,1:length(variable_head)]
  #assign column names
  colnames(timeloop) <- c("Date",variable_head[-1])
  timeloop$Date <- as.Date(timeloop$Date, format= "%Y-%m-%d")
  timeloop$year <- format(as.Date(timeloop$Date), format= "%Y")

  WaterBalance2 <- timeloop %>%
    group_by(year)%>%
    summarise(
      Precip = sum(precip),
      Ice_runoff = sum(iceRunoff),
      actET = sum(actET),
      Annual_RD1 = sum(catchmentRD1_w),
      Annual_RD2 = sum(catchmentRD2_w),
      Annual_RG1 = sum(catchmentRG1_w),
      Annual_RG2 = sum(catchmentRG2_w),
      Snowmelt_Out_G = sum(snowMelt),
      Glacier_runoff = sum(glacierRunoff), #glacierRunoff=snowmeltg+icerunoff+rainrunoff
      SnowMelt_G = sum(snowMelt_G), #plus iceRunoff
      Rain_runoff = sum(rainRunoff)
    )

  a <- which(colnames(WaterBalance2)== "Precip")
  b <- which(colnames(WaterBalance2)== "Ice_runoff")
  c <- which(colnames(WaterBalance2)== "actET")
  d <- which(colnames(WaterBalance2)== "Annual_RD1")
  e <- which(colnames(WaterBalance2)== "Annual_RD2")
  f <- which(colnames(WaterBalance2)== "Annual_RG1")
  g <- which(colnames(WaterBalance2)== "Annual_RG2")
  h <- which(colnames(WaterBalance2)== "Snowmelt_Out_G")

  WaterBalance3 <- WaterBalance2 %>%
    mutate(
      Input = rowSums(.[a:b]),
      Total_Discharge = rowSums(.[d:g]),
      Output = rowSums(.[c:g]),
      PERofET_inPrecip = (actET/Precip*100),
      PERofRD1_inQ = (Annual_RD1/Total_Discharge*100),
      PERofRD2_inQ = (Annual_RD2/Total_Discharge*100),
      PERofRG1_inQ = (Annual_RG1/Total_Discharge*100),
      PERofRG2_inQ = (Annual_RG2/Total_Discharge*100),
      PERofIce_inGlaRunoff = (Ice_runoff/Glacier_runoff*100),
      PERofSnowMelt_G_inGlaRunoff = (SnowMelt_G/Glacier_runoff*100),
      PERofRainRunof_inGlaRunoff = (Rain_runoff/Glacier_runoff*100)
    )

  #Visulization of the Input and Output Variables
  yr <- which(colnames(WaterBalance3)== "year")
  WaterBal3 = select (WaterBalance3, -yr)

  WaterBal3 <- WaterBal3 %>%
    summarise_all(funs(mean))

  WatBalance <- WaterBal3 %>%
    gather(Input,Output,Glacier_runoff, key="WatBal", value="Value")

  WatBalance <- reshape2::melt(WatBalance, id.vars="WatBal")

  a1 <- filter(WatBalance, WatBal == "Input" & variable == "Precip")
  a2 <- filter(WatBalance, WatBal == "Input" & variable == "Ice_runoff")
  a3 <- filter(WatBalance, WatBal == "Output" & variable == "Annual_RD1")
  a4 <- filter(WatBalance, WatBal == "Output" & variable == "Annual_RD2")
  a5 <- filter(WatBalance, WatBal == "Output" & variable == "Annual_RG1")
  a6 <- filter(WatBalance, WatBal == "Output" & variable == "Annual_RG2")
  a7 <- filter(WatBalance, WatBal == "Output" & variable == "actET")
  a8 <- filter(WatBalance, WatBal == "Glacier_runoff" & variable == "SnowMelt_G")
  a9 <- filter(WatBalance, WatBal == "Glacier_runoff" & variable == "Ice_runoff")
  a10 <- filter(WatBalance, WatBal == "Glacier_runoff" & variable == "Rain_runoff")
  a11 <- filter(WatBalance, WatBal == "Glacier_runoff" & variable == "Snowmelt_Out_G")

  WatBalance2 <- rbind(a1, a2,a3,a4, a5, a6, a7,a8,a9,a10,a11)

  cols <- c(Precip="deepskyblue", Ice_runoff="blue4",
            Annual_RD1="brown", Annual_RD2="darkgoldenrod4", Annual_RG1= "goldenrod3", Annual_RG2= "dimgray", actET= "chocolate3",
            SnowMelt_G="darkolivegreen3", Ice_runoff="blue4", Rain_runoff="gold", Snowmelt_Out_G= "green")

  title <- as_labeller(c(Input="Input",Output="Output", Glacier_runoff="Snow and Glaicer"))

  return(ggplot(WatBalance2, aes(variable,value)) +
           geom_col(position="identity", fill= cols) +
           scale_color_manual(values = cols)+
           theme_bw()+ ylab("mm") + xlab("") +
           facet_wrap(.~WatBal, scales= "free", labeller= title))
}


#'Export the data
#'
#'Export the summary data in the Output folder
#'
#'@param TimeLoop Output from J2K model
#'
#'@return Saves the summary of the waterbalance in the folder
#'
#'@examples
#'WatBal_writesummary<- write.csv("Info_On_Water_Balance.csv" in Output folder)
#'
#'@export




WatBal_writesummary <- function(TimeLoop){
  #read header names
  variable_head <- unlist(strsplit(scan(paste(Output,"TimeLoop.dat",sep=""),"",nlines = 1,skip = 5,sep = "\n"),split = "\t"))
  #read data
  timeloop <- fread(paste(Output,"TimeLoop.dat",sep=""),skip = 10)
  timeloop <- timeloop[,1:length(variable_head)]
  #assign column names
  colnames(timeloop) <- c("Date",variable_head[-1])
  timeloop$Date <- as.Date(timeloop$Date, format= "%Y-%m-%d")
  timeloop$year <- format(as.Date(timeloop$Date), format= "%Y")

  #For the analysis of the Input and output variables
  #Removing the years with less than 360 years of data
  timeloop <- timeloop %>%
    add_count(year) %>%
    filter(n > 360)

  #Visulization of the Input and Output variables
  WaterBalance2 <- timeloop %>%
    group_by(year)%>%
    summarise(
      Precip = sum(precip),
      Ice_runoff = sum(iceRunoff),
      actET = sum(actET),
      Annual_RD1 = sum(catchmentRD1_w),
      Annual_RD2 = sum(catchmentRD2_w),
      Annual_RG1 = sum(catchmentRG1_w),
      Annual_RG2 = sum(catchmentRG2_w),
      Snowmelt_Out_G = sum(snowMelt),
      Glacier_runoff = sum(glacierRunoff), #glacierRunoff=snowmeltg+icerunoff+rainrunoff
      SnowMelt_G = sum(snowMelt_G), #plus iceRunoff
      Rain_runoff = sum(rainRunoff)
    )

  a <- which(colnames(WaterBalance2)== "Precip")
  b <- which(colnames(WaterBalance2)== "Ice_runoff")
  c <- which(colnames(WaterBalance2)== "actET")
  d <- which(colnames(WaterBalance2)== "Annual_RD1")
  e <- which(colnames(WaterBalance2)== "Annual_RD2")
  f <- which(colnames(WaterBalance2)== "Annual_RG1")
  g <- which(colnames(WaterBalance2)== "Annual_RG2")
  h <- which(colnames(WaterBalance2)== "Snowmelt_Out_G")

  WaterBalance3 <- WaterBalance2 %>%
    mutate(
      Input = rowSums(.[a:b]),
      Total_Discharge = rowSums(.[d:g]),
      Output = rowSums(.[c:g]),
      PERofET_inPrecip = (actET/Precip*100),
      PERofRD1_inQ = (Annual_RD1/Total_Discharge*100),
      PERofRD2_inQ = (Annual_RD2/Total_Discharge*100),
      PERofRG1_inQ = (Annual_RG1/Total_Discharge*100),
      PERofRG2_inQ = (Annual_RG2/Total_Discharge*100),
      PERofIce_inGlaRunoff = (Ice_runoff/Glacier_runoff*100),
      PERofSnowMelt_G_inGlaRunoff = (SnowMelt_G/Glacier_runoff*100),
      PERofRainRunof_inGlaRunoff = (Rain_runoff/Glacier_runoff*100),
      PERofSnowOutG_inQ = (Snowmelt_Out_G/Total_Discharge*100)
    )
  #Saving the information as a csv file in the same folder
  print(write.csv(WaterBalance3, paste(Output,"Info_On_Water_Balance.csv"), row.names=TRUE))

  print("THE FILE IS SAAVED IN Output FOLDER")
  #Selecting some parameters to display in the Console at the end
  WaterBalance4 <- WaterBalance3 %>%
    select(year,PERofET_inPrecip,PERofRD1_inQ, PERofIce_inGlaRunoff, PERofSnowOutG_inQ)%>%
    mutate_if(is.numeric, round, 2)

  #Displaying the percentage in the Console itself
  print(kable(WaterBalance4, format = "pandoc", caption = "Brief summary of Water-balance in percentage"))
}
