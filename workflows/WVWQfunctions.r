
################################################################
# Write TDG input files for python SYS-TDG script for Willamette EIS
# e.g., Spill_TDG_SYSTDG_lite_graph_Will_v20201119.py
WriteTDG_inputs <- function(addTrbOtgs=TRUE){
  #Write input files for each project
  # prjCds<-list(BCLO=list(q1='DET',q2='BIG',minQperSWgate=1050),
  #              SSFO=list(q1='GREEN',q2='FOST'),
  #              CGRO=list(q1='COUGAR'),
  #              DEXO=list(q1='LOOK',q2='DEXT'))

  AddTurbineOutages <- function(prj){  # The project to analyze, e.g., 'DET'

    # the assumed percentage of planned outage rate (e.g., 0.03)
    ####################
    #Modify outlet flows to account for turbine outages
    ####################

    modProjTurbDays <- function(prj,prjPrc){
      print(prj)
      qCols <- grep(prj,colnames(qpo))
      qpowCols <- grep('POW',colnames(qpo)[qCols])
      qpo$TurbineDays <- qpo[,qCols[qpowCols]] !=0
      TurbineOutage <- rep(F,nrow(qpo))
      WFOPotgDays <- format(qpo$Date,'%m') %in% c('12','01','02')
      for(y in unique(qpo$Year)){
        # yi are the days to have power in a given year
        # Incoporate unplanned outages ( yiup)
        #print(y)
        yi <- yiup <- qpo$Year %in% y & qpo$TurbineDays
        if(any(yi)){
          #set.seed(42) In the future, better to set.seed so that this randomness can be recreated
          yiup[which(yiup)[sample(length(which(yiup)),ceiling(prjPrc*length(which(yi))))]] <- FALSE
          TurbineOutage[yi & !yiup] <- TRUE
        }
        # Incoporate planned outages (yip)
        yip <- yiup & WFOPotgDays
        if(any(yip) & any(yi)){
          nPlndOtgDys <- length(which(yip))
          nOtgDys <- ceiling(PlndOtgPrc*length(which(yi)))
          if(nPlndOtgDys < nOtgDys){
            next
          }
          yip[which(yip)[sample(nPlndOtgDys,nOtgDys)]] <- FALSE
          TurbineOutage[yiup & WFOPotgDays & !yip] <- TRUE
        }
      }
      prjOtgDysChk <- length(which(TurbineOutage))
      #if(grepl('DET',prj)){browser()}

      print(paste('Added', prjOtgDysChk,'(',
                  round((length(which(TurbineOutage)) / length(which(qpo$TurbineDays)))*100),
                  'should =',(prjPrc +PlndOtgPrc)*100 ,'percent) outage days at',prjNm))
      qCols <- grep(prj,colnames(qpo))
      qpowCols <- grep('POW',colnames(qpo)[qCols])
      qspillCols <- grep('SPILLWAY',colnames(qpo)[qCols])
      #qROCols <- grep('OUTLET|POOLFLOW\\.SPILL',colnames(qpo)[qCols])[1]
      qROCols <- grep('OUTLET|UROFLOW|LOWER|ROFLOW',colnames(qpo)[qCols])[1]
      if(grepl('HCR',prjNm)){
        #browser()
        qROCols <- grep('SPILL|REGULATED|ROFLOW',colnames(qpo)[qCols])[1]
      }

      if(grepl('GPR',prjNm)){
        #browser()
      }
      #if(grepl('HILLS',prj)){browser()}
      if(any(TurbineOutage)){
        if(prj %in% c('DET','GREEN','LOOK','HILLS','COUGAR')){
          #spCrest <- data.frame(DET = 1451,GPR = 969 ,LOP = 888)
          #spDays <- qpo[,grep('POOLELEV',colnames(qp)[qCols])] > spCrest[,prj]
          # Augment RO flow
          qpo[TurbineOutage,qCols[qROCols]] <- qpo[TurbineOutage,qCols[qROCols]] +
            qpo[TurbineOutage,qCols[qpowCols]]
        }else{
          # Augment Spillway flow
          qpo[TurbineOutage,qCols[qspillCols]] <- qpo[TurbineOutage,qCols[qspillCols]] +
            qpo[TurbineOutage,qCols[qpowCols]]
        }
        # Set Power Flow to zero for these days
        qpo[TurbineOutage,qCols[qpowCols]] <- 0
      }
      return(qpo[,!grepl('Year|TurbineDays|STOR|ELEV',colnames(qpo))])
    } # End modProjTurbDays

    qpo <- qp
    qpo$Year <- format(qp$Date,'%Y')
    prj = prjCds[[p]][['q1']]
    prjPrc <- FrcdOtgPrcnts$EstPrcTurbOutPrYr[FrcdOtgPrcnts$Project == TDGproj[[p]][['q1']]]
    prjNm <-  unlist(TDGproj)[unlist(TDGproj) %in% TDGproj[[p]][['q1']]]
    prjNmTdg <- substr(names(prjNm),0,4)
    ppath <- file.path(TDGmodPath,prjNmTdg)
    if(!dir.exists(ppath)){dir.create(ppath)}
    #print(prjNm)
    qpo <- modProjTurbDays(prj=prj,prjPrc = prjPrc)
    if(length(prjCds[[p]])>1){
      # Alter the Re-reg dam turbine days if there is one
      qpo$Year <- format(qp$Date,'%Y')
      prj = prjCds[[p]][['q2']]
      prjPrc <- FrcdOtgPrcnts$EstPrcTurbOutPrYr[FrcdOtgPrcnts$Project == TDGproj[[p]][['q2']]]
      #prjNm <-  unlist(TDGproj)[unlist(TDGproj) %in% TDGproj[[p]][['q2']]]
      qpo <- modProjTurbDays(prj=prj,prjPrc = prjPrc)
    }
    return(qpo)
  } # End AddTurbineOutages



  for(p in 1:length(prjCds)){
    ppath <- file.path(TDGmodPath,names(prjCds)[p])
    if(!dir.exists(ppath)){dir.create(ppath)}
    print(paste('Writing files to',ppath))

    # Subset the flows for each project
    qp <- Q4tdgAll[,grep(paste0(c('Date',prjCds[[p]][['q1']],prjCds[[p]][['q2']]),collapse = '|'),colnames(Q4tdgAll))]
    qp <- qp[!apply(apply(qp,2,is.na),1,any),]
    pnm <- file.path(ppath,names(prjCds)[p])
    swCols <- grepl('Date|SPILL',colnames(qp))
    # Write all other gate flow files for TDG estimation
    # Write peaking/hydroplant reservoir flows
    # Access turbine outages
    # TurbOtgs <- read_csv(ObsTurbOutFlnm) %>%
    #   mutate(name = as.factor(name))
    # Apply turbine outages
    if(length(TDGproj[[p]])==1){
      prjp <- TDGproj[[p]][['q1']]
    }else{
      prjp <- TDGproj[[p]][['q2']]
    }
    if(grepl('CGRO',names(prjCds)[p])){
      # browser()
    }
    if(addTrbOtgs){
      qpo <- AddTurbineOutages(prj = prjp)
    }else{
      qpo <- qp
    }
    q1Cols <- grep(prjCds[[p]][['q1']],colnames(qpo))
    q1nms <- c('q1_powerhouse',  'q1_spill_SW','q1_spill_RO')
    q1powCols <- grepl('POW',colnames(qpo)[q1Cols])
    q1spillCols <- grepl('SPILLWAY',colnames(qpo)[q1Cols])
    if(any(prjCds[[p]] == "HILLS")){
      q1ROCols <- grepl('SPILL|ROFLOW',colnames(qpo)[q1Cols])
    }else{
      q1ROCols <- grepl('OUTLET|ROFLOW|LOWER',colnames(qpo)[q1Cols])
    }
    if(!any(q1spillCols)){
      q1nms <- q1nms[!grepl('SW',q1nms)] 
    }
    
    for(qo in 1:length(q1Cols)){
      modFname <- file.path(ppath,paste0(q1nms[qo],'.csv'))
      if(any(q1powCols) & grepl('pow',q1nms[qo])){ # Power total
        write_csv(qpo[,c(1,q1Cols[q1powCols])],modFname,col_names=F)
      }
      # Spillway
      if(any(q1spillCols) & grepl('SW',q1nms[qo])){
        write_csv(qpo[,c(1,q1Cols[q1spillCols])],modFname,col_names=F)
      }
      # RO
      if(any(q1ROCols) & grepl('RO',q1nms[qo])){
        q1ro <- qpo[,c(1,q1Cols[q1ROCols])]
        if(length(which(q1ROCols))>1){
          q1ro$q1_ro <- apply(q1ro[,-1],1,sum)
        }else{
          q1ro$q1_ro <- q1ro[,-1]
        }
        write_csv(q1ro[,c('Date','q1_ro')],modFname,col_names=F)
      }
    }# End writing first reservoir flow data
    # Write re-regulating reservoir flows
    if(length(prjCds[[p]])>1){
       q2Cols <- grep(prjCds[[p]][['q2']],colnames(qp))
       q2nms <- c('q2_powerhouse', 'q2_spilltotal')
       # Apply turbine outages
       # if(addTrbOtgs){
       #   qpo <- AddTurbineOutages(prj = TDGproj[[p]][['q2']])
       # }else{
       #   qpo <- qp
       # }
      q2Cols <- q2Cols[!is.na(q2Cols)]
      for(qo in 1:length(q2Cols)){
        modFname <- file.path(ppath,paste0(q2nms[qo],'.csv'))
        if(grepl('pow',q2nms[qo])){ # Power total
          q2powCols <- grep('POW',colnames(qpo)[q2Cols])
          write_csv(qpo[,c(1,q2Cols[q2powCols])],modFname,col_names=F)
        }else{ # Spill total
          if(names(prjCds)[p]=='BCLO'){
            # Write the spillway gate flow files for BCLO
            # Number of gates is only a factor at BCL as of 2020-11-16
            # at BCL Dam Assume 0.75 feet minimum gate opening at 1190-1200 ft elevation ops range for WV EIS, which is about 1000cfs per gate
            sbnms <- c('q2_sb1', 'q2_sb2', 'q2_sb3')
            qp2 <- qpo[,swCols] %>%
              select(Date,contains(prjCds[[p]][['q2']]))
            qp2$q2_sb1 <- qp2[,2] # Includes all flows below minimum gate opening
            qp2$q2_sb2 <- qp2$q2_sb3 <- 0
            sb12i <- qp2[,2] > prjCds[[p]][['minQperSWgate']]*2 & qp2[,2] < prjCds[[p]][['minQperSWgate']]*3
            sb123i <- qp2[,2] > prjCds[[p]][['minQperSWgate']]*3
            qp2$q2_sb1[sb12i] <- qp2$q2_sb2[sb12i] <- qp2[sb12i,2] / 2
            qp2$q2_sb1[sb123i] <- qp2$q2_sb2[sb123i] <- qp2$q2_sb3[sb123i] <- qp2[sb123i,2] / 3
            # Check the total spill flow adds up to match the RESSIM total
            qspdif <- round(mean(apply(qp2[,c('q2_sb1', 'q2_sb2', 'q2_sb3')],1,sum) - qp2[,2]),2)
            print(paste('Mean difference btwn calculated total and RESSIM total =',qspdif))
            if(abs(qspdif)>100){
              print('Stopping; check calculations splitting spillbay flow!!!')
              break
            }
            # Write the individual spillbay flows for re-reg reservoir
            for(sb in 1:length(sbnms)){
              modFname <- file.path(ppath,paste0(sbnms[sb],'.csv'))
              write_csv(qp2[,match(c('Date',sbnms[sb]),colnames(qp2))],modFname,col_names=F)

            }
          } # End BCLO if statement
          # Write the total spill flow file for re-reg reservoir
          q2spillCols <- grepl('SPILLWAY',colnames(qpo)[q2Cols])
          modFname <- file.path(ppath,paste0(q2nms[qo],'.csv'))
          write_csv(qpo[,c(1,q2Cols[q2spillCols])],modFname,col_names=F)
        }# End for loop for re-reg dam outflow data type (power/spill)
      }# End for loop for re-reg dam
    }# End writing re-reg flow data
    # Write S Santiam inflow file for Foster
    finm <- 'q2_flow_in'
    modFname <- file.path(ppath,paste0(finm,'.csv'))
    fosInCols <- (grepl(paste0('^',prjCds[[p]][['q2']],'.*INFLOW'),colnames(qpo)))
    if(any(fosInCols)){
      write_csv(qpo[,(grepl('Date',colnames(qpo)) | fosInCols)],
                modFname,col_names=F)
    }

  }# End Project loop
}



################################################################
# Read TDG output from python SYS-TDG script for Willamette EIS
# e.g., Spill_TDG_SYSTDG_lite_graph_Will_v20201119.py
GetTDG <- function(TDGdir,ReadObs=F,RdctnPrc=0.0){
  library(tidyr)
  library(dplyr)
  prjCds2 <- list(BCLO=list(q1='DET',q2='BCL'),
                  SSFO=list(q1='GPR',q2='FOS'),
                  CGRO=list(q1='CGR'),
                  HCRO=list(q1='HCR'),
                  DEXO=list(q1='LOP',q2='DEX'))
  for(p in 1:length(prjCds2)){
    print(names(prjCds2)[p])
    #ppath <- file.path(TDGwriteDir,altDir,DSSFileNames[[1]][['partF']],names(prjCds2)[p])
    ppath <- file.path(TDGdir,names(prjCds2)[p])
    pfls <- list.files(ppath)
    p_tdgOut_flnm <- pfls[grep('Qspill_TDG_',pfls)]
    print(paste('Reading', p_tdgOut_flnm))
    p_tdg <- read_csv(file.path(ppath,p_tdgOut_flnm),show_col_types = F)
    colnames(p_tdg) <- gsub('_RO','RO',gsub('_SW','SW',gsub('q1|z1',prjCds2[[p]][[1]],colnames(p_tdg))))
    p_tdg$datetime <- as.Date(p_tdg$datetime,format='%m/%d/%Y %H:%M')
    p_tdg <- p_tdg[order(p_tdg$datetime),]
    if(grepl('CGR|HCR',prjCds2[[p]][[1]])){
      colnames(p_tdg) <- gsub(paste0(prjCds2[[p]][[1]],'_spillRO'),
                              paste0(prjCds2[[p]][[1]],'_spilltotal'),colnames(p_tdg))
    }
    if(length(prjCds2[[p]])>1){
      colnames(p_tdg) <- gsub('q2|z2',prjCds2[[p]][[2]],colnames(p_tdg))
    }else{
      p_tdg$z2_TDGmix <- NULL
    }
    colnames(p_tdg)[-match('datetime',colnames(p_tdg))] <-
      paste0(colnames(p_tdg)[-match('datetime',colnames(p_tdg))],'_Estimated')

    # Subtract some TDG if Measure 174 is in place
    if(length(RdctnPrc)>1|is.data.frame(RdctnPrc)){
      RdctnPrc_p <- RdctnPrc[,match(names(prjCds2)[p],colnames(RdctnPrc))]
    }else{
      RdctnPrc_p <- RdctnPrc
    }
    if(RdctnPrc_p>0){
      tdgCol <- which(grepl(last(prjCds2[[p]]),colnames(p_tdg)) & grepl("TDGmix",colnames(p_tdg)))
      tdgAdgRws <- p_tdg[,tdgCol] > (100 + RdctnPrc_p*100)
      p_tdg[tdgAdgRws,tdgCol] <- p_tdg[tdgAdgRws,tdgCol] - RdctnPrc_p*100
    }

    if(ReadObs){
      # Read observed TDG
      obsPath <-file.path(tdgModDirs,c("North_Santiam_v2","Sourth_Santiam","Cougar","HCR", "Mid_Fork_Willamette" )[p])
      obsFls <- list.files(obsPath,pattern='.csv')
      pObs_tdgOut_flnm <- obsFls[grep('Qspill_TDG_',obsFls)]
      pObs_tdg <- read_csv(file.path(obsPath,pObs_tdgOut_flnm),skip=3,show_col_types = F)
      colnames(pObs_tdg) <- gsub('_RO','RO',gsub('_SW','SW',gsub('q1|z1',prjCds2[[p]][[1]],colnames(pObs_tdg))))
      pObs_tdg$datetime <- as.Date(pObs_tdg$datetime,format='%m/%d/%Y %H:%M')
      if(length(prjCds2[[p]])>1){
        colnames(pObs_tdg) <- gsub('q2|z2',prjCds2[[p]][[2]],colnames(pObs_tdg))
      }else{
        colnames(pObs_tdg) <- gsub('z2',prjCds2[[p]][[1]],colnames(pObs_tdg))
      }
      colnames(pObs_tdg) <- gsub('_tw_TDG','_TDGobs',colnames(pObs_tdg))
      colnames(pObs_tdg)[-match('datetime',colnames(pObs_tdg))] <-
        paste0(colnames(pObs_tdg)[-match('datetime',colnames(pObs_tdg))],'_Observed')

      p_tdg <- merge(pObs_tdg,p_tdg,by='datetime',all=T)
    }
    #if(p==1){browser()}
    p_tdg <- p_tdg[,grepl('datetime|spill|power|TDGmix',colnames(p_tdg))]
    p_tdgL <- p_tdg %>% #[!apply(apply(p_tdg,2,is.na),1,any),]  %>%
      pivot_longer(cols= -datetime,names_sep='_',names_to=c('name','dtype','EstObs')) %>%
      mutate(dtype = gsub('TDGmix','TDG',dtype))

    print(str(p_tdgL))
    # pObs_tdgL <- pObs_tdg[,grepl('datetime|spill|power|TDGobs',colnames(pObs_tdg))] %>%
    #   pivot_longer(cols= -datetime,names_sep='_',names_to=c('name','dtype')) %>%
    #   mutate(EstObs = 'Observed',
    #          dtype = gsub('TDGobs','TDG',dtype))
    #merge estimated and observed
    p_tdgL <- p_tdgL %>% #rbind(p_tdgL,pObs_tdgL) %>%
      dplyr::rename(Date = datetime) %>%
      mutate(Year = format(Date, "%Y"),
             DateGen = as.Date(paste0('1935-',format(Date,'%m-%d'))),
             MonthDay = floor_date(DateGen, "month"))
    p_tdgL <- p_tdgL[!apply(apply(p_tdgL,2,is.na),1,any),]
    d15<-as.numeric(format(p_tdgL$DateGen,'%d'))>15
    p_tdgL$MonthDay[d15] <- floor_date(p_tdgL$DateGen[d15], "month") + days(14)
    mdys <- format(p_tdgL$MonthDay,'%m-%d')
    p_tdgL <- p_tdgL %>% select(-DateGen) %>%
      mutate(MonthDay = factor(format(MonthDay,'%m-%d'),ordered=T,levels=mnthdysAll),
             Alt = as.factor(DSSFileNames[[1]]$partF),
             dtype = as.factor(dtype),
             EstObs = as.factor(EstObs),
             name = as.factor(name))

    if(p==1){
      tdg <- p_tdgL
    }else{
      tdg <- rbind(tdg,p_tdgL)
    }
  }
  #tdg$value[tdg$dtype=="TDG"] <- ifelse(tdg$value[tdg$dtype=="TDG"]<100,100,tdg$value[tdg$dtype=="TDG"])

  tdg$Year <- as.factor(tdg$Year)
  return(tdg)
}

#list2env(m,envir = parent.frame())
LoadAltW2npts <- function(RoptFlNm){
  load(RoptFlNm)
  #print(str(as.data.frame(X)))
  return(X)
}

CompareAltW2npts <- function(){
  pltNm <- paste0('W2nptCompOps_',gsub(', ','_',toString(unique(w2npts$Alt))),'.png')
  w2npts <- w2npts %>% mutate(Alt = as.factor(Alt))
  MyPalsmall <- MyPal[grepl(paste0(names(altDirs),collapse ='|'), names(MyPal))]
  for(site in levels(w2npts$Site)){
    print(paste("Making",site,"plots"))
    #site = levels(w2npts$Site)[1]

    if(nrow(w2npts %>% filter(grepl(site,Site)))==0){
      next
    }
    OpsByProj<-ggplot(w2npts %>% filter(grepl(site,Site)  & grepl('FLOW',Param)) %>%
                        mutate(Param = as.factor(gsub('FLOW|_FLOW','',Param))),
                      aes(x=as.Date(JDAY, origin = as.Date("2018-12-31")),
                          y=value/cfs2cms, colour=Alt,fill=Alt)) +
      geom_line(alpha=0.6,size=1) +
      facet_grid(Param ~ Year,scales='free') +
      ylab('Flow, in cfs') +
      scale_x_date(name = 'Month-Day',date_labels = "%b",date_breaks = '1 month') +
      scale_y_continuous(sec.axis = sec_axis(~ . *cfs2cms,name = 'Flow, in cms')) +
      scale_colour_manual(values = MyPalsmall) +
      theme(text = element_text(size=12),
            legend.position="top",#"none", #
            #legend.key=element_blank(),
            panel.background = element_rect(fill = "white", colour = "grey50"),
            panel.grid.major = element_line(colour = "grey"),
            axis.text=element_text(size=12),
            axis.text.y = element_text(size=8),
            #strip.text.x = element_blank(),
            legend.title = element_blank(),
            strip.text.y = element_text(margin = margin(2,0,2,0, "cm"),angle = 0),
            axis.text.x = element_text(size=8,angle = 45, hjust = 1)
      ) +
      ggtitle(paste(site,"Project Outflows"))
    #OpsByProj
    ggsave(plot = OpsByProj,device = 'png',width=10,height=5,
           filename=file.path(compDir,paste0(site,'_',pltNm )))

    TotQBySite<-ggplot(w2npts %>% filter(grepl(site,Site)  &
                                           (JDAY > 121 & JDAY < 275) &
                                           grepl('OUTFLOW',Param)) %>%
                         mutate(Param = as.factor(gsub('AT|FLOW|_FLOW','',Param))),
                       aes(x=as.Date(JDAY, origin = as.Date("2018-12-31")),
                           y=value/cfs2cms, colour=Alt,fill=Alt)) +
      geom_line(alpha=0.6,size=1) +
      facet_grid(Year~.,scales='fixed') +
      ylab('Flow, in cfs') +
      scale_x_date(name = 'Month-Day',date_labels = "%b",date_breaks = '1 month') +
      scale_y_continuous(sec.axis = sec_axis(~ . *cfs2cms,name = 'Flow, in cms'),trans='log10') +
      scale_colour_manual(values = MyPalsmall) +
      theme(text = element_text(size=12),
            legend.position="top",#"none", #
            #legend.key=element_blank(),
            panel.background = element_rect(fill = "white", colour = "grey50"),
            panel.grid.major = element_line(colour = "grey"),
            axis.text=element_text(size=12),
            axis.text.y = element_text(size=8),
            #strip.text.x = element_blank(),
            legend.title = element_blank(),
            strip.text.y = element_text(margin = margin(2,0,2,0, "cm"),angle = 0),
            axis.text.x = element_text(size=8,angle = 45, hjust = 1)
      ) +
      ggtitle(paste(site,"Streamflow [cfs]"))
    ggsave(plot = TotQBySite,device = 'png',width=10,height=5,
           filename=file.path(compDir,paste0(site,'_',gsub('Ops','TotQ',pltNm ))))


    if(nrow(w2npts %>% filter(grepl(site,Site)  & grepl('RULE',Param)))==0){
      next
    }

    OutletElvs <- qgtConfig$CenterElvs[match(site,names(qgtConfig$CenterElvs))][[1]]
    OutletElvs <- OutletElvs[!grepl('SWS|LRO|FSS|Low|Up|WTC2|WTC3|Penstock',names(OutletElvs))]
    OutletElvs <- foreach(alt = levels(w2npts$Alt),.combine='rbind') %do%{
        data.frame(Outlet = names(OutletElvs),y = as.numeric(OutletElvs),Alt = alt)
      } %>% mutate(Alt = as.factor(Alt))
    w2nptsElvs <- w2npts %>% filter(grepl(site,Site),grepl('ELEV',Param))
    if(exists('WSELVmeas')){
      w2nptsElvs <- w2nptsElvs %>% 
        full_join(WSELVmeas %>% 
                    filter(grepl(site,Site)) %>%
                    mutate(value = value*ft2m,
                           JDAY = as.numeric(strftime(Date, format = "%j")),
                           Alt = 'Measured') %>%
                    select(-`Date Time`,-Date)) %>%
        mutate(Alt = factor(Alt,ordered = T,
                            levels = names(altDirs)))
    }
      
    ElvByProj<-ggplot(w2nptsElvs,
                      aes(x=as.Date(JDAY, origin = as.Date("2018-12-31")),
                          y=value/ft2m, colour=Alt, group=Alt)) +
      geom_line(data = w2npts %>% filter(grepl(site,Site)  & grepl('RULE',Param)),
                aes(x=as.Date(JDAY, origin = as.Date("2018-12-31")),
                    y=value),color ='grey',size=1.4) +
      geom_line(alpha=0.6,size=1) +
      geom_hline(aes(yintercept = y),color = 'black',linetype = 'dashed',
                 size=0.6,alpha = 0.6,data = OutletElvs) +
      geom_text(aes(x=as.Date(min(w2npts$JDAY)+40, origin = as.Date("2018-12-31")),
                    y=y,label = Outlet, vjust = -0.5),
                data = OutletElvs,color = 'black') +
      facet_grid(~ Year,scales='free') +
      ylab('Lake Level, in feet') +
      scale_y_continuous(sec.axis = sec_axis(~ . *ft2m,name = 'Lake Level, in m')) +
      scale_x_date(name = 'Month',date_labels = "%b",date_breaks = '1 month') +
      scale_colour_manual(values = MyPalsmall) +
      theme(text = element_text(size=12),
            legend.position="top",#"none", #
            legend.key=element_blank(),
            panel.background = element_rect(fill = "white", colour = "grey50"),
            panel.grid.major = element_line(colour = "grey"),
            axis.text=element_text(size=12),
            axis.text.y = element_text(size=8),
            #strip.text.x = element_blank(),
            legend.title = element_blank(),
            strip.text.y = element_text(margin = margin(2,0,2,0, "cm"),angle = 0),
            axis.text.x = element_text(size=8,angle = 45, hjust = 1)
      ) +
      ggtitle(paste(site,"Lake Levels"))
    ElvByProj
    print(file.path(compDir,paste0(site,'_',gsub('Ops','Elvs',pltNm ))))
    ggsave(plot = ElvByProj,device = 'png',width=10,height=5,
           filename=file.path(compDir,paste0(site,'_',gsub('Ops','Elvs',pltNm ))))
  }


  for(param in levels(w2npts$Param)[
      grepl('SALEM|VIDA|ALBANY',levels(w2npts$Param))]){
      print(paste("Making",param,"Total Flow plot"))
      #site = levels(w2npts$Site)[1]
      if(nrow(w2npts %>% filter(grepl(param,Param)))==0){
        next
      }
      TotQByParam<-ggplot(w2npts %>% filter(grepl(param,Param)  &
                                             (JDAY > 121 & JDAY < 275) ) %>%
                         mutate(Param = as.factor(gsub('AT|FLOW|_FLOW','',Param))),
                       aes(x=as.Date(JDAY, origin = as.Date("2018-12-31")),
                           y=value/cfs2cms, colour=Alt,fill=Alt)) +
      geom_line(alpha=0.6,size=1) +
      facet_grid(Year~.,scales='fixed') +
      ylab('Flow, in cfs') +
      scale_x_date(name = 'Month-Day',date_labels = "%b",date_breaks = '1 month') +
      scale_y_continuous(sec.axis = sec_axis(~ . *cfs2cms,name = 'Flow, in cms'),trans='log10') +
      scale_colour_manual(values = MyPalsmall) +
      theme(text = element_text(size=12),
            legend.position="top",#"none", #
            #legend.key=element_blank(),
            panel.background = element_rect(fill = "white", colour = "grey50"),
            panel.grid.major = element_line(colour = "grey"),
            axis.text=element_text(size=12),
            axis.text.y = element_text(size=8),
            #strip.text.x = element_blank(),
            legend.title = element_blank(),
            strip.text.y = element_text(margin = margin(2,0,2,0, "cm"),angle = 0),
            axis.text.x = element_text(size=8,angle = 45, hjust = 1)
      ) +
      ggtitle(paste(gsub('AT|FLOW|_FLOW','',param),"Streamflow [cfs]"))
      ggsave(plot = TotQByParam,device = 'png',width=10,height=5,
           filename=file.path(compDir,paste0(param,'_',gsub('Ops','TotQ',pltNm ))))
  } #End Param loop
} # End Function

ReadAltTDG <- function(var){
  altNums <- which(!names(altDirs)%in%'Measured')
  xsum <- foreach(alt = altNums,.combine='rbind') %do%{ #:length(altDirs)
    load(file.path(altDirs[[alt]][['fld']],altDirs[[alt]][['RTDGoptFlNm']]))
    xsum_a <- get(var)
    xsum_a$Alt <- names(altDirs)[alt] #gsub('ltom','Alt5',
    #str(xsum_a)
    return(xsum_a)
  }
  return(xsum %>% mutate(Alt = factor(Alt,ordered=T,levels=names(altDirs))))
}


CompareAltTDG <- function(){
  TDGproj = as.list(rep(NA,5))
  TDGproj[[1]] <-list(q1='DET',q2='BCL')
  TDGproj[[2]] <-list(q1='GPR',q2 = 'FOS')
  TDGproj[[3]] <-list(q1='CGR')
  TDGproj[[4]] <-list(q1='HCR')
  TDGproj[[5]] <-list(q1='LOP',q2='DEX')
  names(TDGproj) <- c('BCL','FOS','CGR','HCR','DEX')
  TDGproj[[2]] <- NULL # Leave out Green Peter-Foster

  print('Making TDG Comparison Plots')
  pltNm <- paste0('TDGPrcTbl_',gsub(', ','_',toString(unique(tdgOpsSum$Alt))),'.png')

  tdgOpsSum <- tdgOpsSum %>% mutate(Alt = as.factor(Alt)) %>%
    filter(name %in% names(TDGproj),!MonthDay %in% c("07-01", "07-15" ,"08-01", "08-15", "09-01")) %>%
    pivot_longer(cols=-c('Alt','MonthDay','name'),names_to ='Stat')
  MyPalsmall <- MyPal[grepl(paste0(unique(tdgOpsSum$Alt),collapse ='|'),names(MyPal))]


  # annual and Month-Day TDG summary
  tdgPrcTbl <- ggplot(tdgOpsSum %>% filter(grepl('95',Stat)),
                     aes(y=name,x=MonthDay,fill=value,label=round(value))) +
    geom_tile(alpha=0.8) +
    geom_text(color='white',size=3) +
    facet_grid(Alt~.,scales = 'free',
               labeller = labeller(Stat = statLabs)) +
    ylab('95th percentile of TDG (% Sat)') +
    theme(axis.title.x=element_blank(),
          strip.text.y = element_text(angle = 0),
          legend.position ='none',
          axis.text.x=element_text(angle=45,hjust=1))
  tdgPrcTbl
  ggsave(plot=tdgPrcTbl,device = 'png',width=10,height=5,
         filename=file.path(compDir,pltNm))

  # Compare 95th % TDG with another alternative
  tdgPrcCmpTbl <- ggplot(tdgOpsSum %>% filter(grepl('95',Stat),MonthDay %in% "Year"),
                      aes(y=name,x=MonthDay,fill=value,label=round(value))) +
    geom_tile(alpha=0.8) +
    geom_text(color='white',size=3) +
    facet_grid(.~Alt,scales = 'free',
               labeller = labeller(Stat = statLabs)) +
    ylab('95th percentile of TDG (% Sat)') +
    theme(axis.title.x=element_blank(),
          strip.text.y = element_text(angle = 0),
          legend.position ='none',
          axis.text.x=element_text(angle=45,hjust=1))
  tdgPrcCmpTbl
  ggsave(plot=tdgPrcCmpTbl,device = 'png',width=10,height=5,
         filename=file.path(compDir,gsub('Tbl','CmpTbl',pltNm)))

  # Month-Day summarizing number of days over 110
  tdgExcTbl <- ggplot(tdgExcMD %>%
                      filter(name %in% names(TDGproj),!MonthDay %in% c("07-01", "07-15" ,"08-01", "08-15", "09-01")),
                      aes(y=name,x=MonthDay,fill=AvgNDaysAbv110,label=round(AvgNDaysAbv110))) +
    geom_tile(alpha=0.8) +
    geom_text(color='white',size=3) +
    facet_grid(.~Alt,scales = 'free',
               labeller = labeller(Stat = statLabs)) +
    ylab('Average Number of Days above 110% TDG') +
    theme(axis.title.x=element_blank(),
          strip.text.y = element_text(angle = 0),
          legend.position ='none',
          legend.title = element_blank(),
          axis.text.x=element_text(angle=45,hjust=1))
  tdgExcTbl
  # ggsave(plot=tdgExcTbl,device = 'png',width=10,height=5,
  #        filename=file.path(compDir,gsub('PrcTbl','ExcCmpTbl',pltNm)))

  #Summarize number of days over 110
  TDGsumExcByMD <- tdgAll  %>%
    filter( dtype=='TDG') %>%
    group_by(Year,Alt,MonthDay,name) %>%
    dplyr::summarize_at(vars(value),function(x){length(which(x>110))}) %>%
    mutate(name = factor(name,levels=Sts3ltr,ordered=T))

  # Compare Box plots of number of days above 110 TDG with another alternative
  tdgExcBx <- ggplot(TDGsumExcByMD %>%
                     filter(name %in% names(TDGproj),
                            !MonthDay %in% c("07-01", "07-15" ,"08-01", "08-15", "09-01")),
                   aes(y=value,x=MonthDay,colour=Alt)) +
    geom_boxplot() +
    facet_grid(name~.,scales = 'fixed') +
    scale_colour_manual(values = MyPalsmall) +
    ylab('Number of Days above 110% TDG') +
    theme(legend.position ='top',
          legend.title = element_blank(),
          axis.text.x=element_text(angle=45,hjust=1))
  tdgExcBx
  ggsave(plot=tdgExcBx,device = 'png',width=8,height=5,
         filename=file.path(compDir,gsub('PrcTbl','ExcCmpBox',pltNm)))

  # Annual summarizing number of days over 110
  tdgExcTblYr <- ggplot(tdgExcYr %>%
                      filter(name %in% names(TDGproj)),
                      aes(y=name,x=MonthDay,fill=AvgNDaysAbv110,label=round(AvgNDaysAbv110))) +
    #geom_tile(alpha=0.8) +
    geom_tile(alpha=0.6) +
    geom_text(color='black',size=4) +
    #geom_text(color='white',size=3) +
    facet_grid(.~Alt,scales = 'free',
               labeller = labeller(Stat = statLabs)) +
    ylab('Average Number of Days above 110% TDG') +
    theme(axis.title.x=element_blank(),
          strip.text.y = element_text(angle = 0),
          legend.position ='none',
          legend.title = element_blank(),
          axis.text.x=element_text(angle=45,hjust=1))
  tdgExcTblYr
  ggsave(plot=tdgExcTblYr,device = 'png',width=10,height=5,
         filename=file.path(compDir,gsub('PrcTbl','ExcCmpTblYr',pltNm)))

  Thresh <- data.frame(ExcCat1 = 'Negligible',
                       ExcVal1 = 25,
                       ExcCat2 = 'Minor',
                       ExcVal2 = 50,
                       ExcCat3 = 'Moderate',
                       ExcVal3 = 100,
                       ExcCat4 = 'Major'
  )
  # Categorize impacts based on number of days over 110
  DefineTDGImpacts <- function(x){
    x <- as.numeric(x)
    xa <- abs(x)
    if(xa < Thresh$ExcVal1){
        i = Thresh$ExcCat1
    }else{
      if(xa < Thresh$ExcVal2){
        i = Thresh$ExcCat2
      }else{
        if(xa < Thresh$ExcVal3){
          i = Thresh$ExcCat3
        }else{
          i = Thresh$ExcCat4
        }
      }
    }
    if(i != 'Negligible'){
      if(x > 0){
        i = paste(i,'Adv')
      }else{
        i = paste(i,'Ben')
      }
    }
    return(i)
  }
    # Check Differences from Baseline in each category and define impact
    tdgExcYr <- tdgExcYr %>% select(-MonthDay)
    tdgExcYrDif  <- tdgExcYr %>%
        group_by(Alt) %>%
        mutate(DifFromBaseline = AvgNDaysAbv110-
                 tdgExcYr %>%
                 filter(Alt == 'Baseline') %>%
                 select(AvgNDaysAbv110)) %>%
      select(-AvgNDaysAbv110) %>%
      filter(Alt != 'Baseline')

  tdgExcYrDifTab <- ggplot(tdgExcYrDif %>%
                             filter(name %in% names(TDGproj)),
                       aes(y=name,x=Alt,fill=DifFromBaseline$AvgNDaysAbv110,
                           label=round(DifFromBaseline$AvgNDaysAbv110))) +
    geom_tile(alpha=0.6) +
    geom_text(color='black',size=4) +
    ylab('') +
      scale_fill_gradient2(high = "orange",mid = "grey",  low = "darkblue",midpoint = 0) +
      ggtitle('Annual Difference in Number of Days Above 110% TDG Compared to Baseline') +
      theme(axis.title.x=element_blank(),
            strip.text.y = element_text(angle = 0),
            legend.position ='none',
            legend.title = element_blank(),
            axis.text.x=element_text(angle=45,hjust=1))
  tdgExcYrDifTab
  ggsave(plot=  tdgExcYrDifTab,device = 'png',width=10,height=5,
           filename=file.path(compDir,gsub('PrcTbl','ExcCmpTblYrDif',pltNm)))


  tdgImpacts <- data.frame(tdgExcYrDif) %>%
      group_by(name,Alt) %>%
      mutate(Impact = factor(DefineTDGImpacts(DifFromBaseline$AvgNDaysAbv110),
                             levels = c("Major Ben","Moderate Ben" ,
                                        "Minor Ben", "Negligible",
                                        "Minor Adv", "Moderate Adv",
                                        "Major Adv"),ordered = T))
  tdgImpct <- ggplot(tdgImpacts,
                        aes(y=name,x=Alt,fill=Impact,
                            label=gsub('Major','Maj',
                                       gsub('Minor','Min',
                                            gsub('Moderate','Mod',Impact))))) +
    geom_tile(alpha=0.6) +
    geom_text(color='black',size=4) +
    scale_fill_manual(values = ImpactPal) +
    ggtitle('TDG Impacts Based on Annual Number of Days Above 110% TDG') +
    theme(axis.title.x=element_blank(),
          strip.text.y = element_text(angle = 0),
          #legend.position ='none',
          #legend.title = element_blank(),
          axis.text.x=element_text(angle=45,hjust=1))
    tdgImpct
  ggsave(plot=tdgImpct,device = 'png',width=10,height=5,
         filename=file.path(compDir,gsub('PrcTbl','Impacts',pltNm)))
  #Write Summary metrics for Alt2 formulation
  write.csv(tdgExcYr %>% filter(name %in% names(TDGproj)) %>%
              mutate(AvgNDaysAbv110 = round(AvgNDaysAbv110)) %>%
              pivot_wider(id_cols = name,names_from = Alt,
                          values_from = AvgNDaysAbv110) ,
            row.names = F,
            file = file.path(compDir,gsub('.png','.csv',gsub('PrcTbl','ExcCmpTblYr',pltNm))))

  # Annual summarizing number of spill days
  SpllDysTblMoDy <- tdgAll %>%
    filter(#name %in% names(TDGproj),
      grepl('spill',dtype), #dtype == 'spilltotal',
      !MonthDay %in% c("07-01", "07-15" ,"08-01", "08-15", "09-01")) %>%
    group_by(Year,MonthDay,Alt,name) %>%
    dplyr::summarize(NspillDays = length(which(value>0))) %>%
    group_by(MonthDay,Alt,name) %>%
    dplyr::summarize(NspillDays = mean(NspillDays))
  SpllDysTblMoDyPlt <- ggplot(SpllDysTblMoDy,
                        aes(y=name,x=MonthDay,fill=NspillDays,label=round(NspillDays))) +
    geom_tile(alpha=0.6) +
    geom_text(color='black',size=4) +
    # geom_tile(alpha=0.8) +
    # geom_text(color='white',size=3) +
    facet_grid(.~Alt,scales = 'free',
               labeller = labeller(Stat = statLabs)) +
    ylab('Average Number of Days With Spill') +
    theme(axis.title.x=element_blank(),
          strip.text.y = element_text(angle = 0),
          legend.position ='none',
          axis.text.x=element_text(angle=45,hjust=1))
  SpllDysTblMoDyPlt
  # ggsave(plot=SpllDysTblMoDy,device = 'png',width=10,height=5,
  #         filename=file.path(compDir,gsub('PrcTbl','SpillDaysTblMoDy',pltNm)))
  # # Annual summarizing number of spill days
  SpllDysTblYr <- tdgAll %>%
    filter(#name %in% names(TDGproj),
      grepl('spill',dtype), #dtype == 'spilltotal',
      !MonthDay %in% c("07-01", "07-15" ,"08-01", "08-15", "09-01")) %>%
    group_by(Year,Alt,name) %>%
    dplyr::summarize(NspillDays = length(which(value>0))) %>%
    group_by(Alt,name) %>%
    dplyr::summarize(NspillDays = ifelse(all(NspillDays ==0) |all(is.na(NspillDays)),0,mean(NspillDays)))
  SpllDysTblYrPlt <- ggplot(SpllDysTblYr,
                         aes(y=name,x=Alt,fill=NspillDays,label=round(NspillDays))) +
    geom_tile(alpha=0.6) +
    geom_text(color='black',size=4) +
    # geom_tile(alpha=0.8) +
    # geom_text(color='white',size=3) +
    # facet_grid(.~Alt,scales = 'free',
    #            labeller = labeller(Stat = statLabs)) +
    ylab('Average Number of Days With Spill per Year') +
    theme(axis.title.x=element_blank(),
          strip.text.y = element_text(angle = 0),
          legend.position ='none',
          axis.text.x=element_text(angle=45,hjust=1))
  SpllDysTblYrPlt
  ggsave(plot=SpllDysTblYrPlt,device = 'png',width=10,height=5,
         filename=file.path(compDir,gsub('PrcTbl','SpillDaysTblYr',pltNm)))


  # Compare time-series plots of TDG with another alternative
  tdgL1 <- ggplot(tdgAll %>% filter(dtype == 'TDG', name %in% names(TDGproj),
                                     grepl('2011|2012|2013|2014|2015|2016',Year)),
                   aes(x=Date,y=value,colour=Alt)) + #,linetype=Alt
    geom_line() +
    ylim(100,135) +
    facet_grid(name~.,scales = 'fixed') +
    scale_colour_manual(values = MyPalsmall) +
    geom_hline(yintercept=110,color='grey') +
    ylab('TDG (%Sat)') +
    theme(legend.position ='top',
          legend.title = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x=element_text(angle=45,hjust=1))
  tdgL1
  ggsave(plot=tdgL1,device = 'png',width=8,height=5,
         filename=file.path(compDir,gsub('PrcTbl','LineAll',pltNm)))

  # Compare Box plots of TDG with another alternative
  tdgBx1 <- ggplot(tdgAll %>% filter(dtype == 'TDG', name %in% names(TDGproj)),
                         aes(x=value,colour=Alt)) + #,linetype=Alt
    geom_boxplot() +
    xlim(95,135) +
    facet_grid(~name,scales = 'fixed') +
    scale_colour_manual(values = MyPalsmall) +
    geom_vline(xintercept=110,color='grey') +
    xlab('Threshold TDG (%Sat)') +
    theme(legend.position ='top',
          legend.title = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x=element_text(angle=45,hjust=1))
  tdgBx1
  ggsave(plot=tdgBx1,device = 'png',width=8,height=5,
         filename=file.path(compDir,gsub('PrcTbl','BoxAll',pltNm)))

  # Emperical Distribution Function: ECDF
  # Non-exceedence plots
  tdge1 <- ggplot(tdgAll %>% filter(dtype == 'TDG', name %in% names(TDGproj)),
                  aes(x=value,colour=Alt)) + #,linetype=Alt
    stat_ecdf(geom = "step",alpha=0.6,size=1) +
    facet_grid(~name,scales = 'free') +
    #ylim(0.8,1.0) +
    xlim(105,130) +
    scale_colour_manual(values = MyPalsmall) +
    geom_vline(xintercept=110,color='grey',alpha=0.7,size=2) +
    ylab('Percent Of Time Below Threshold TDG (%Sat)') +
    xlab('Threshold TDG (%Sat)') +
    theme(legend.position ='top',
          legend.title = element_blank(),
          axis.text.x=element_text(angle=45,hjust=1))
  tdge1
  ggsave(plot=tdge1,device = 'png',width=8,height=5,
         filename=file.path(compDir,gsub('PrcTbl','Ecdf',pltNm)))

  print(paste('Finished plotting',pltNm))
  print(paste('Writing TDG Excel Output',pltNm))

  library(openxlsx)
  TDGxlsxList <- list()

  TDGxlsxList$TDG_AvgNDaysAbv110 =
    tdgExcYr %>% filter(name %in% names(TDGproj)) %>%
    mutate(AvgNDaysAbv110 = round(AvgNDaysAbv110)) %>%
    pivot_wider(id_cols = name,names_from = Alt,
                values_from = AvgNDaysAbv110) %>% ungroup()
  TDGxlsxList$TDG_ImpactCats = tdgImpacts %>% ungroup() %>%
    select(-DifFromBaseline) %>%
    pivot_wider(names_from = Alt,values_from = Impact)
  TDGxlsxList$SpllDysTblYr = SpllDysTblYr %>%
    pivot_wider(names_from = Alt,values_from = NspillDays) %>%
    ungroup()

  openxlsx::write.xlsx(TDGxlsxList,
                       file = file.path(compDir,gsub('.png','.xlsx',gsub('PrcTbl','TDG_CompTbl',pltNm))),
                       asTable = TRUE,overwrite = T,append=T)

}

ReadAltOps <- function(RoptFlNm){
  load(RoptFlNm)
  # Look at the variable "QMonthSum" to tabulate ops percentiles from each alternative
  return(QMonthSum)
}


ReadMeasWSELVs <- function(){
  fls <- list.files(WSELVobsDir,pattern = '.csv')
  WSELVmeas <- foreach(fl = fls,.combine = 'rbind') %do% {
    yrs = c(2011,2015,2016)
    x <- read_csv(file.path(WSELVobsDir,fl),show_col_types = F)
    colnames(x)[2] <- 'value'
    x <- x %>% mutate(Date = as.Date(`Date Time`),
                      Year = format(Date,'%Y')) %>%
      filter(Year %in% yrs) %>%
      #group_by(Date) %>%
      #summarize(Tdavg = mean(Tmeas)) %>%
      mutate(#Year = format(Date,'%Y'),
        Site = substr(fl,0,3))
    return(x)
  }
  WSELVmeas <- WSELVmeas %>%
    mutate(Site = factor(Site,levels = levels(w2npts$Site),ordered=T),
           Year = factor(Year,levels = levels(w2npts$Year)))
  return(WSELVmeas)
}

calcEmergenceTiming<-function(tout, #tout is a dataframe with the first column as a numeric Julian day
                              atu.day=263, #atu.day is the day in which to start the emergence calculation
                              interpMissing=T,
                              hatchValue=1750
){
  library(tidyr)
  if(interpMissing){
    jd<-1:366
    jdi<-!jd%in%unique(floor(tout$JDAY))
    nmiss <- length(which(jdi))
    if(any(jdi)){
      # fill missing days
      #print(str(tout))
      tout<-data.frame(JDAY=jd,
                       Temp=approx(x=tout[,1],y=tout[,2],xout=jd,rule=2)$y)
    }
  }
  if(floor(tout$JDAY[1])<=2){
    atu.temps<-rbind(tout[tout$JDAY>atu.day,],
                     tout[tout$JDAY<atu.day,])
  }else{
    atu.temps<-tout[tout$JDAY>atu.day,]
  }

  if(mean(atu.temps[,2],na.rm=T)<32){
    atu.temps[,-1]<-atu.temps[,-1]*(9/5)+32 # convert to F
  }
  atu.temps$JDAY[atu.temps$JDAY<atu.day]<-
    atu.temps$JDAY[atu.temps$JDAY<atu.day]+max(atu.temps$JDAY)
  atu.temps<-atu.temps[!apply(apply(atu.temps,2,is.na),1,any),]
  #print(summary(atu.temps))
  cum.dif<-atu.temps
  cum.dif[,-1]<-NA
  if(ncol(atu.temps)>2){
    cum.dif[,-1]<-apply(atu.temps[,-1]-32,2,cumsum)
    atu.d<-apply(cum.dif[,-1],2,function(x){cum.dif[x>=hatchValue,1][1]})
  }else{
    cum.dif[,-1]<-cumsum(atu.temps[,-1]-32)
    atu.d<-cum.dif[cum.dif[,-1]>=hatchValue,1][1]
  }
  atu<-as.data.frame(t(format(as.Date(as.numeric(atu.d),origin=as.Date('2010-12-31')),'%m/%d')),stringsAsFactors=F)
  atu.d<-as.data.frame(t(round(as.numeric(atu.d))),stringsAsFactors=F)
  #return(data.frame(atu=as.character(atu),atu.d=as.numeric(atu.d),nmiss = nmiss))
  return(as.numeric(atu.d))
}

SumEmerge<-function(Tdams){
  # Calc emergence through each year and site with a function
  Atud <- Tdams %>%
    mutate(Location = factor(Location,levels = Sts3ltr,ordered=T),
           Year= factor(Year),Alt = factor(Alt)) %>%
    select(JDAY,TMean,Location, Year,Alt) %>%
    complete(Location, Year, Alt) %>%
    expand_grid(SpawnDay = spawndays) %>%
    group_by(Location, Year, Alt,SpawnDay) %>%
    dplyr::summarise(atu.d = calcEmergenceTiming(tout = data.frame(JDAY = JDAY,T_F = c2f(TMean)),
                                         atu.day = unique(SpawnDay)),
              atu = format(as.Date(atu.d,origin = as.Date('2000-01-01')),'%m/%d'))%>%
    mutate(SpawnDay = as.character(SpawnDay)) %>%
    ungroup()

  # Average over all Spawn Days
  TdmSpawnAvg <- Atud %>%
    group_by(Location,Year,Alt) %>%
    dplyr::summarize(atu.d = mean(atu.d,na.rm=T),
                     atu = format(as.Date(atu.d,origin = as.Date('2000-01-01')),'%m/%d')) %>%
    mutate(SpawnDay = 'Average') %>%
    full_join(Atud)
  # Average over all Years
  TdmYrAvg <- Atud %>%
    group_by(Location,Alt,SpawnDay) %>%
    dplyr::summarize(atu.d = mean(atu.d,na.rm=T),
                     atu = format(as.Date(atu.d,origin = as.Date('2000-01-01')),'%m/%d')) %>%
    mutate(Year = 'Average') %>%
    full_join(TdmSpawnAvg)
  return(TdmYrAvg)
}

#Read Dam outflow files
ReadW2Tdam <- function(fldr){
  resnms <- data.frame(lopdex = '14150000',cgr = '14159500',
                       gpfos = '14187200',det = NA,bcl = '14181500',hcr = '14145500')
  fls7d <- list.files(fldr,recursive = T, pattern='two')
  whichnms <- unlist(sapply(gsub('.opt','',fls7d),function(x){grep(rev(strsplit(x,'_')[[1]])[1],names(resnms))}))
  whichnms <- gsub('.opt','',fls7d) %in% names(whichnms) &
    !grepl('mfwill',fls7d) & #two_17|two_36|
    !grepl('str',fls7d)
  fls7d <- fls7d[whichnms]
  Flfls7d <- list.files(file.path(fldr),pattern='two',full.names = T,recursive = T) #,'dam_outflow_files'
  Flfls7d <- Flfls7d[whichnms]
  Alt <- names(fldr) #gsub('ltom','Alt5',
  print(paste('Alt = ',Alt))
  print(paste('Folder Names: ',names(fldr)))
  if(Alt != 'Measured'){
     Td <- foreach(fl = (1:length(fls7d)),.combine='rbind') %do% { 
      #print(fls7d[fl])
      td <- read_csv(Flfls7d[fl],skip=2,show_col_types = F)[,c(1,2)]
      tdnm <- strsplit(rev(strsplit(fls7d[fl],'\\/')[[1]])[1],'_')
      tdyr <- gsub("\\D+", "",strsplit(fls7d[fl],'\\/')[[1]][2])
      Loc = toupper(gsub('.opt','',rev(tdnm[[1]])[1]))
      if(rev(tdnm[[1]])[2] == '17'){Loc = 'GPR'}
      if(rev(tdnm[[1]])[2] == '80'){Loc = 'FOS'}
      if(rev(tdnm[[1]])[2] == '36'){Loc = 'LOP'}
      if(rev(tdnm[[1]])[2] == '57'){Loc = 'DEX'}
      td <- td %>% mutate(JDAYi = floor(JDAY)) %>%
        group_by(JDAYi) %>%
        dplyr::summarise(JDAY = unique(JDAYi),
                         TMean = mean(`T(C)`),
                         TMax = max(`T(C)`),
                         TMin = min(`T(C)`)) %>%
        mutate(T7dadm = round(rollmean(TMax, 7, na.pad = T, align = "right"),2),
               Location = Loc, Year = tdyr) %>% 
        select(-JDAYi) 
      if(nrow(td)<150){
        feedback <- paste('Alt = ',Alt,'; Loc= ',Loc,'; Year =',tdyr,'; Nrows=',nrow(td))
        print(feedback)
        #write(feedback,logFilenm)
      }
      return(td)
    }

  }else{

    Tmeas7d <- Tmeas %>% filter(!grepl(Sts2SmrzLow,Site))
    Td <- foreach(loc = unique(Tmeas7d$Site),.combine='rbind') %do% {
      print(loc)
      foreach(yr = unique(Tmeas7d$Year),.combine='rbind') %do% {
        ty <- Tmeas7d %>% filter(Year == yr, Site == loc) %>%
          group_by(Date) %>%
          dplyr::summarise(Date = unique(Date),
                           TMean = mean(Tmeas),
                           TMax = max(Tmeas),
                           TMin = min(Tmeas)) %>%
          mutate(T7dadm = round(rollmean(TMax, 7, na.pad = T, align = "right"),2),
                 JDAY = julian(Date,origin = as.Date(paste0(yr,'-01-01')))) %>%
          filter(JDAY>0)
        return(ty)
      }
    }
    
  }
  # # End if/else for measured data
  Td <- Td %>%
     mutate(Alt = names(fldr))
  return(Td)
}


#Summarize amount of time off/on temp target
CalcW2TtDiff <- function(Tdams = NA){
  #rm(TtargDifLoc)
  for(a in levels(Tdams$Alt)){
    print(paste('Calculating temp difference from target for',a))
    for(l in levels(Tdams$Location)){
      #print(l)
      for(y in levels(Tdams$Year)){
        #print(y)
        for(ttT in c('RA','W2','TMDL','ORStnd')){
          #print(ttT)
           #merge daily temp data with temp target
            ttsl <- ttAll %>% filter(grepl(l,Location),
                                     grepl(ttT,ttType)
                                     )
            TdamsSub <- Tdams %>% 
              filter(grepl(y,Year),
                     grepl(l,Location),
                     grepl(a,Alt)) 
            ttLoc <- ttsl %>%
              full_join(TdamsSub %>%
                          dplyr::rename('TempF' = 'TMean') %>%
                          mutate(TempF = c2f(TempF)),by = 'JDAY')
            if(length(which(!is.na(ttLoc$Targ)))>2 & 
               length(which(!is.na(ttLoc$TempF)))>2){
              ttLoc <- ttLoc %>%
                mutate(Targ = approx(x=JDAY,y=Targ,xout = JDAY,rule=2)$y,
                       TempF = approx(x=JDAY,y=TempF,xout = JDAY,rule=2)$y) %>%
                group_by(JDAY = round(JDAY)) %>%
                dplyr::summarize(Targ = mean(Targ, na.rm=T),TempF = mean(TempF)) %>%
                mutate(Location = l) %>%
                mutate(Diff = TempF - Targ,
                       Month = factor(format(as.Date(JDAY,origin = as.Date('2010-12-31')),'%b'),levels = month.abb,ordered=T),
                       TargNm = ttT,
                       Year = y,
                       Alt = a)
            }else{
              #print("skipping")
              next
            }
            if(!exists('TtargDifLoc')){
              TtargDifLoc <- ttLoc
            }else{
              TtargDifLoc <- rbind(TtargDifLoc,ttLoc)
            }
            rm(ttLoc)
        } # End Ttarg loop
      }# End Year loop
    }# End Location loop
  }# End Alt loop
  TtargDifLoc <- TtargDifLoc %>%
    mutate(Year = as.factor(Year),Location = as.factor(Location),
           Alt = Alt)
  print(ncol(TtargDifLoc))
  return(  TtargDifLoc)
}


PlotW2TempByYear <- function(ttType = 'W2'){
  pltNm <- paste0('TempByYear',ttType,'Targs.png')
  MyPalsmall <- MyPal[grepl(paste0(names(altDirs),collapse ='|'), names(MyPal))]
  idcols = c('JDAY','Location','Month','Year')
  rm(TtargDifBaselineLoc)
  for(Loc in levels(TtargDifLocs$Location)){
    TtargDifBaselineLoca <- TtargDifLocs %>% filter(#!grepl('Baseline',Alt),
                                          grepl(ttType,TargNm),
                                          grepl(Loc,Location)) 
    if(nrow(TtargDifBaselineLoca)>0){
      TtargDifBaselineLoca <- TtargDifBaselineLoca     %>%
        select(-Diff,-TargNm) %>%
        pivot_wider(id_cols = idcols,
                    names_from = Alt,values_from = TempF)
      x <- foreach(a = levels(TtargDifLocs$Alt),.combine = 'rbind') %do% {
        TtargDifBaselineLoca$Diff <- unlist(TtargDifBaselineLoca[,a] - TtargDifBaselineLoca[,'Baseline'])
        data.frame(TtargDifBaselineLoca[,c(idcols,'Diff')],Alt = a)
      }
      if(!exists('TtargDifBaselineLoc')){
        TtargDifBaselineLoc <- x
      }else{
        TtargDifBaselineLoc <- rbind(TtargDifBaselineLoc,x)
      }   
      rm(TtargDifBaselineLoca, x)
    }else{
      next
    }
  }
  TtargDifBaselineLoc <- TtargDifBaselineLoc %>%
    mutate(Alt = factor(Alt,levels = names(altDirs))) #,ordered = T))
  
  for(Loc in levels(TtargDifLocs$Location)){
    print(paste("Making",Loc,"Temp Timeseries Plots"))
    TtargDifLoc <- TtargDifLocs %>% filter(grepl(Loc,Location),
                            grepl(ttType,TargNm),
                            #Alt %in% names(altDirs),
                            !grepl('Jan|Feb|Mar',Month),
                            JDAY >0) %>%
      dplyr::rename(DifFromTarget = Diff) %>%
      pivot_longer(cols = c('TempF','DifFromTarget'),names_to = 'Param') %>% #,'Targ',
      mutate(Param = factor(Param,levels =  c('TempF','DifFromTarget'),ordered=T))
    if(nrow(TtargDifLoc)==0){next}
    # Plot Temp difference from target for each alternative
    TempByProj<-ggplot(TtargDifLoc,
                       aes(x=as.Date(JDAY,origin = as.Date("2018-12-31")),
                           y=value, colour=Alt)) +
      geom_path(alpha=0.6,size=1) +
      geom_path(data = TtargDifLocs %>% filter(grepl(Loc,Location),
                                          grepl(ttType,TargNm)#,
                                          #!grepl('Jan|Feb|Mar',Month)
                                          ) %>%
                  mutate(value = Targ,Param = 'TempF'),
                color = 'black',
                aes(x=as.Date(JDAY, origin = as.Date("2018-12-31")),
                    y=value, colour=Alt),alpha=0.6,size=1) +
      geom_hline(data = TtargDifLocs %>% filter(grepl(Loc,Location),
                                                #!grepl('Jan|Feb|Mar',Month),
                                                grepl(ttType,TargNm),
                                                JDAY >0) %>%
                   dplyr::rename(DifFromTarget = Diff) %>%
                   pivot_longer(cols = c('TempF','DifFromTarget'),names_to = 'Param') %>% #,'Targ',
                   mutate(Param = factor(Param,levels =  c('TempF','DifFromTarget'),ordered=T)) %>%
                   filter(!grepl('TempF',Param)) %>%
                   mutate(value = 0),
                 aes(yintercept = 0),
                 color='darkgrey',size=1,
                 alpha = 0.6) +
      geom_hline(data = TtargDifLocs %>% filter(grepl(Loc,Location),
                                           #!grepl('Jan|Feb|Mar',Month),
                                           grepl(ttType,TargNm),
                                           JDAY >0) %>%
                   dplyr::rename(DifFromTarget = Diff) %>%
                   pivot_longer(cols = c('TempF','DifFromTarget'),names_to = 'Param') %>% #,'Targ',
                   mutate(Param = factor(Param,levels =  c('TempF','DifFromTarget'),ordered=T)) %>%
                   filter(!grepl('DifFromTarget',Param)) %>%
                   mutate(value = c2f(18)),
                 aes(yintercept = c2f(18)),
                 color='grey',size=1,
                 linetype="dotted",alpha = 0.6) +
      facet_grid(Param ~ Year,scales='free') +
      scale_colour_manual(values = MyPalsmall) +
      ylab('7dADM Temperature, Degrees F') +
      scale_x_date(name = 'Month',date_labels = "%b",date_breaks = '1 month',
                   limits =as.Date(c(91,365),origin = as.Date("2018-12-31"))) +
      #scale_y_continuous(sec.axis = sec_axis(~ . f2c(),name = 'Temperaature, Deg C')) +
      theme(text = element_text(size=12),
            legend.position="top",#"none", #
            #legend.key=element_blank(),
            panel.background = element_rect(fill = "white", colour = "grey50"),
            panel.grid.major = element_line(colour = "grey"),
            axis.text=element_text(size=12),
            axis.text.y = element_text(size=8),
            #strip.text.x = element_blank(),
            legend.title = element_blank(),
            strip.text.y = element_text(margin = margin(2,0,2,0, "cm"),angle = 0),
            axis.text.x = element_text(size=8,angle = 45, hjust = 1)
      ) +
      ggtitle(paste(Loc," Water Temperature"))
    #TempByProj
    
    ggsave(plot = TempByProj,device = 'png',width=10,height=5,
           filename=file.path(compDir,paste0(Loc,'_',pltNm )))
    
    if(ttType == 'W2'){
      #if(Loc == 'FOS') browser()
      # Plot Temp difference between Baseline and each alternative
      TDifBaselineLoc <- TtargDifBaselineLoc %>% #filter(Alt != 'Baseline') %>%
        full_join(TtargDifLocs %>% select(-Diff)) %>%
        dplyr::rename(DifFromBaseline = Diff) %>%
        pivot_longer(cols = c('TempF','DifFromBaseline'),names_to = 'Param') %>% #,'Targ',
        mutate(Param = factor(Param,levels =  c('TempF','DifFromBaseline'),ordered=T)) %>%
        filter(grepl(Loc,Location),
               grepl(ttType,TargNm),
               !grepl('Jan|Feb|Mar',Month)
               )
      BaselineTempDifByProj<-ggplot(TDifBaselineLoc,# %>%
                                 #filter(Alt %in% names(altDirs)),
                               aes(x=as.Date(JDAY, origin = as.Date("2018-12-31")),
                                   y=value, colour=Alt)) +
        geom_line(alpha=0.6,size=1) +
        # Add line for Temp target
        geom_path(data = TtargDifLocs %>%
                    filter(grepl(Loc,Location),
                           grepl(ttType,TargNm)
                           #!grepl('Jan|Feb|Mar',Month)
                           ) %>%
                    mutate(value = Targ,Param = 'TempF'),
                  color = 'black',
                  aes(x=as.Date(JDAY, origin = as.Date("2018-12-31")),
                      y=value, colour=Alt),alpha=0.6,size=1) +
        # Add line for ORStnd
        geom_path(data = TtargDifLocs %>%
                    filter(grepl(Loc,Location),
                           grepl('ORStnd',TargNm)
                    ) %>%
                    mutate(value = Targ,Param = 'TempF'),
                  
                  aes(x=as.Date(JDAY, origin = as.Date("2018-12-31")),
                      y=value, colour=Alt),
                  color='grey',size=1,
                  linetype="dotted",alpha = 0.6) +
        facet_grid(Param ~ Year,scales='free') +
        scale_colour_manual(values = MyPalsmall) +
        ylab('7dADM Temperature, Deg F') +
        scale_x_date(name = 'Month',date_labels = "%b",date_breaks = '1 month',
                     limits =as.Date(c(91,365),origin = as.Date("2018-12-31"))) +
        # geom_hline(data = filter(TDifBaselineLoc,!grepl('DifFromBaseline',Param)),
        #            aes(yintercept = c2f(18)),color='grey',size=1,
        #            linetype="dotted",alpha = 0.6) +
        geom_hline(data = filter(TDifBaselineLoc,!grepl('TempF',Param)),
                   aes(yintercept = 0),color='darkgrey',size=1,
                   alpha = 0.6) +
        #scale_y_continuous(sec.axis = sec_axis(~ . -32 * (5/9),name = 'Temperature, Deg C')) +
        theme(text = element_text(size=12),
              legend.position="top",#"none", #
              #legend.key=element_blank(),
              panel.background = element_rect(fill = "white", colour = "grey50"),
              panel.grid.major = element_line(colour = "grey"),
              axis.text=element_text(size=12),
              axis.text.y = element_text(size=8),
              #strip.text.x = element_blank(),
              legend.title = element_blank(),
              strip.text.y = element_text(margin = margin(2,0,2,0, "cm"),angle = 0),
              axis.text.x = element_text(size=8,angle = 45, hjust = 1)
        ) +
        ggtitle(paste(Loc," Water Temperature"))
      #BaselineTempDifByProj
      ggsave(plot =  BaselineTempDifByProj,device = 'png',width=10,height=5,
             filename=file.path(compDir,paste0(Loc,'DifBaseline_',pltNm )))
    }
  }# End Loc loop
  
}

TtExc <- function(TtargDifLocs){ #summarize percent time off target temp
  th<-2 # Threshold temp [deg F] for "off" target
  #ttLocSum <-
  return(ttLocSum)
}


CompQinResOpsW2 <- function(){
  W2ResModDirs <- list.dirs(w2ModDir,recursive = F)
  W2ResModDirs <- W2ResModDirs[grep('det|gpfos|cgr|hcr|lop',W2ResModDirs)]
  W2ResModDirsShrt <- unlist(lapply(strsplit(W2ResModDirs,'\\/'),function(x){rev(x)[1]}))

  ##### Read inflows from RES-SIM and W2 and compare #####
  QinW2 <- foreach(f = 1:length(W2ResModDirs),.combine='rbind') %do%{
    fldrs <- list.dirs(W2ResModDirs[f],recursive = F)
    fldrs <- fldrs[!grepl('_1per_forscenarios',fldrs)]
    fldrsShrt <- unlist(lapply(strsplit(fldrs,'\\/'),function(x){rev(x)[1]}))
    fldrs
    qil <- qinLines[[rev(strsplit(W2ResModDirs[f],'\\/')[[1]])[1]]]

    qin1 <- foreach(m = 1:length(fldrs)[noMissingAlts],.combine='rbind') %do%{
      if(grepl('gp|lop',rev(strsplit(W2ResModDirs[f],'\\/')[[1]])[1])){
        qil1 <- qil[[1]]; qil2 <- qil[[2]]
      }else{
        qil1 <- qil
      }
      xm1 <- get.w2.inflows(path=fldrs[m],q.rows=qil1)
      xm1[14,]
      xm1$Year <- substr(fldrsShrt[m],nchar(fldrsShrt[m]) - 3,nchar(fldrsShrt[m]))
      return(xm1)
    }
    qin1$Site <- gsub('fos|dex|20|1|5|6','', W2ResModDirsShrt[f])
    return(qin1)
  }
  QinW2w <- QinW2 %>% pivot_wider(id_cols = c('JDAY'),
                        values_from = 'Total.Qin',
                        names_from = c('Site','Year'))
  write.csv(QinW2,file = file.path(w2dir,'WVEISW2PeakingProjTotQin.csv'),
            row.names = F)
  write.csv(QinW2w,file = file.path(w2dir,'WVEISW2PeakingProjTotQinWide.csv'),
            row.names = F)

}


ReadMeasTemps <- function(){
  tfls <- list.files(tobsDir,pattern = 'T.csv')
  Tmeas <- foreach(tfl = tfls,.combine = 'rbind') %do% {
    yrs = c(2011,2015,2016)
    x <- read_csv(file.path(tobsDir,tfl),show_col_types = F)
    colnames(x)[2] <- 'Tmeas'
    x <- x %>% mutate(Date = as.Date(`Date Time`),
                      Year = format(Date,'%Y')) %>%
      filter(Year %in% yrs) %>%
      #group_by(Date) %>%
      #summarize(Tdavg = mean(Tmeas)) %>%
      mutate(#Year = format(Date,'%Y'),
             Site = substr(tfl,0,4))
    return(x)
  }
  return(Tmeas)
}

#Read Dam outflow files
ReadW2Qdam <- function(fldr){
  resnms <- data.frame(lopdex = '14150000',cgr = '14159500',#gpr = '14186200',
                       gpfos = '14187200',det = NA,bcl = '14181500',hcr = '14145500')
  fls7d <- list.files(file.path(fldr),pattern='qwo',recursive = T)
  whichnms <- unlist(sapply(gsub('.opt','',fls7d),function(x){grep(rev(strsplit(x,'_')[[1]])[1],names(resnms))}))
  whichnms <- gsub('.opt','',fls7d) %in% names(whichnms) &
    !grepl('mfwill',fls7d) & #two_17|two_36|
    !grepl('str',fls7d)
  fls7d <- fls7d[whichnms]
  Flfls7d <- list.files(file.path(fldr),
                        pattern='qwo',full.names = T,recursive = T)
  Flfls7d <- Flfls7d[whichnms]
  # changeName <- function(x){gsub('17_gpfos','17_gpr',
  #                                gsub('80_gpfos','80_fos',
  #                                   gsub('36_lopdex','36_lop',
  #                                        gsub('57_lopdex','57_dex',x))))
  # }
  Alt <- names(fldr)
  Qsp <- foreach(fl = (1:length(fls7d)),.combine='rbind') %do% {
    qdnm <- strsplit(rev(strsplit(fls7d[fl],'\\/')[[1]])[1],'_')
    qdyr <- gsub("\\D+", "",strsplit(fls7d[fl],'\\/')[[1]][2])
    Loc = toupper(gsub('.opt','',rev(qdnm[[1]])[1]))
    if(rev(qdnm[[1]])[2] == '17'){Loc = 'GPR'}
    if(rev(qdnm[[1]])[2] == '80'){Loc = 'FOS'}
    if(rev(qdnm[[1]])[2] == '36'){Loc = 'LOP'}
    if(rev(qdnm[[1]])[2] == '57'){Loc = 'DEX'}
    if(any(names(qgtConfig[[1]])==Loc)){
      qotnms <- names(qgtConfig[[1]][[Loc]])
    }else{
      browser()
    }
    print(paste(fls7d[fl],Loc,qdyr))
    qf <- read_csv(Flfls7d[fl],skip=3,show_col_types = F,
                   col_names = c('JDAY','QTOT',qotnms))
    qf <- qf[,!apply(apply(qf,2,is.na),2,all)]
    # Define percent spill
    spillCols <- grep('Spillway|Weir|FWWS|DTnnl|RO',colnames(qf))
    if(length(spillCols)>1){
      qf$SpillTot <- apply(data.frame(qf)[,spillCols],1,sum)
    }else{
      qf <- qf %>% mutate(SpillTot = data.frame(qf)[,spillCols])
    }
    qf <- qf %>%
      mutate(Date = as.Date(JDAY,origin = as.Date(paste0(qdyr,'-01-01'))),
             Month = factor(format(Date,'%b'),levels = month.abb),
             PrcSpl = SpillTot / QTOT) %>%
      select(c(Month,PrcSpl)) %>%
      group_by(Month) %>%
      dplyr::summarize(PrcSpl = round(mean(PrcSpl)*100)) %>%
      mutate(Year = qdyr,Site = Loc)
    return(qf)
  }

  Qsp <- Qsp %>%
    mutate(Location = factor(Site,levels = names(qgtConfig$CenterElvs),ordered=T),
           Year= factor(Year)) %>%
    select(-Site)
  # Monthly average over all years
  QspMoAvg <- Qsp %>%
     group_by(Location,Month) %>%
     dplyr::summarize(PrcSpl = mean(PrcSpl,na.rm=T)) %>%
      mutate(Alt = Alt)
  return(QspMoAvg)
}

ReadRuleCurves <- function(RuleCurveDir){
  flsrc <- list.files(RuleCurveDir,pattern = 'ft')
  names(flsrc) <- unlist(lapply(flsrc,function(x){strsplit(x,split = '_')[[1]][1]}))
  rcs <- foreach(rc = 1:length(flsrc),.combine = 'rbind') %do% {
    print(names(flsrc)[rc])
    rcp <- read_csv(file.path(RuleCurveDir,flsrc[rc]),skip=3,col_names = c('JDAY','value'),show_col_types = F)
    rcp <- rcp %>% mutate(Site = names(flsrc)[rc],
                          Param = 'RULE') #Year = '2011',
    return(rcp)
  }
  return(rcs)
}


ReadTMDLTargs <- function(TMDLtargdir){
  flstt <- list.files(TMDLtargdir)
  names(flstt) <- unlist(lapply(flstt,function(x){strsplit(x,split = '_')[[1]][1]}))
  tts <- foreach(tt = 1:length(flstt),.combine = 'rbind') %do% {
    print(names(flstt)[tt])
    ttp <- read_csv(file.path(TMDLtargdir,flstt[tt]),show_col_types = F)
    ttp <- ttp %>% mutate(Targ = unlist(c2f(`TMDL target (C)`)))
    ttpNew <- data.frame(month = 1:12)
    ttp <- ttp %>% select(month,Targ) %>% 
      full_join(ttpNew) %>%
      mutate(Targ = ifelse(is.na(Targ),min(Targ,na.rm = T),Targ))
    ttp <- ttp[order(ttp$month),]
    ttp2 <- ttp %>% 
      mutate(Date = as.Date(paste0('2019-',formatC(month,width = 2,flag = '0'),
                                   '-15')),
             JDAY = julian(Date, origin = as.Date('2018-12-31')) 
             ) %>%
      select(-Date,-month)
    ttp2 <- approx(x=ttp2$JDAY,y=ttp2$Targ,xout = c(1:366),ties='ordered',rule=2)
    ttp <- data.frame(JDAY = ttp2$x,Targ = ttp2$y,
                      Location = gsub('DET','BCL',names(flstt)[tt]))
    rm(ttp2)
    return(ttp)
  }
  
  return(tts %>% mutate(ttType = 'TMDL')
         # Location = factor(gsub('HCR','HCRO',gsub('DEX','DEXO',gsub('CGR','CGRO',gsub('FOS','SSFO',gsub('BCL','BCLO',Location))))),
                        #                   levels = rev(c("HCRO","DEXO","CGRO","SSFO" ,"BCLO")),ordered=T),
  )
}

ReadOrStStnds <- function(TMDLtargdir){
  flstt <- list.files(TMDLtargdir)
  names(flstt) <- unlist(lapply(flstt,function(x){strsplit(x,split = '_')[[1]][1]}))
  tts <- foreach(tt = 1:length(flstt),.combine = 'rbind') %do% {
    print(names(flstt)[tt])
    ttp <- read_csv(file.path(TMDLtargdir,flstt[tt]),show_col_types = F)
    ttp <- ttp %>% mutate(Targ = unlist(c2f(`ORStStndrd(C)`)))
    #ttpNew <- data.frame(month = 1:12)
    # ttp <- ttp %>% select(month,Targ) %>% 
    #   full_join(ttpNew) %>%
    #   mutate(Targ = ifelse(is.na(Targ),min(Targ,na.rm = T),Targ))
    # ttp <- ttp[order(ttp$month),]
    # ttp2 <- ttp %>% 
    #   mutate(Date = as.Date(paste0('2019-',formatC(month,width = 2,flag = '0'),
    #                                '-15')),
    #          JDAY = julian(Date, origin = as.Date('2018-12-31')) 
    #   ) %>%
    #   select(-Date,-month)
    ttp2 <- approx(x=ttp$JDAY,y=ttp$Targ,xout = c(1:366),ties='ordered',rule=2)
    ttp <- data.frame(JDAY = ttp2$x,Targ = ttp2$y,
                      Location = gsub('DET','BCL',names(flstt)[tt]))
    rm(ttp2)
    return(ttp)
  }
  
  return(tts %>% mutate(ttType = 'ORStnd')
         # Location = factor(gsub('HCR','HCRO',gsub('DEX','DEXO',gsub('CGR','CGRO',gsub('FOS','SSFO',gsub('BCL','BCLO',Location))))),
         #                   levels = rev(c("HCRO","DEXO","CGRO","SSFO" ,"BCLO")),ordered=T),
  )
}

ReadW2Targs <- function(w2TtargDir){
  flstt <- list.files(w2TtargDir)
  flstt <- flstt[-grep('2017WATER',flstt)]
  names(flstt) <- unlist(lapply(flstt,function(x){strsplit(x,split = '_')[[1]][1]}))
  tts <- foreach(tt = 1:length(flstt),.combine = 'rbind') %do% {
    ttp <- read.fwf(file.path(w2TtargDir,flstt[tt]),widths = c(8,8),
                    skip = 3,col.names = c('JDAY','Targ'))
    ttp$Targ <- c2f(ttp$Targ)
    ttp2 <- approx(x=ttp$JDAY,y=ttp$Targ,xout = c(1:366),ties='ordered',rule=2)
    ttp <- data.frame(JDAY = ttp2$x,Targ = ttp2$y,
                      Location = gsub('HCL','HCR',
                                    gsub('DET','BCL',
                                        gsub('LOP','DEX',
                                             #gsub('GPR','FOS',
                                                  names(flstt)[tt])))
                      )
    rm(ttp2)
    return(ttp)
  }
  return(tts %>% mutate(#Location = factor(Location,ordered=T,
                        #                  levels = c('BCL','GPR','FOS','CGR','HCR','DEX')),
                        ttType = 'W2'))
}


ReadRATargs <- function(RATargsdir){
  tts <- read_csv(RATargsdir,show_col_types = F) %>%
    mutate(JDAY = julian(mdy(`Date Time`),origin = as.Date('2018-12-31'))) %>%
    select(-`Date Time`)
  cls <- c(1:ncol(tts))[!grepl('JDAY',colnames(tts))]
  tts2 <- foreach(tt = cls,.combine = 'rbind') %do% {
    ttc <- approx(x=as.numeric(tts$JDAY),y=unlist(tts[,tt]),
                  xout = c(1:366),ties='ordered',rule=2)
    ttp <- data.frame(JDAY = ttc$x,Targ = ttc$y,
                      Location = colnames(tts)[tt])
    return(ttp)
  }
  tts2 <- tts2 %>%
    mutate(Location = unlist(sapply(Location,function(x) strsplit(x,'\\.')[[1]][1])),
           Location = gsub("BCLO","BCL",
                           gsub("CGRO","CGR",
                                gsub("HCRO","HCR",
                                     gsub("DEXO","DEX",
                                          gsub('GPRO','GPR',
                                            gsub("SSFO","FOS" ,Location)))))), 
           #Location = factor(Location,ordered=T,
          #                   levels = c('BCL','FOS','CGR','HCR','DEX')),
           ttType = 'RA') 
  return(tts2)
}

ReadW2Tdavg <- function(fldr,Tdams = NA){
  fls7d <- list.files(file.path(fldr,'7d_avgs_forfishmodelers'))
  Flfls7d <- list.files(file.path(fldr,'7d_avgs_forfishmodelers'),full.names = T)
  alt <- names(fldr)
  
  # Summarize Exceedences for each period
  SumExc <- function(Tmean){ #summarize percent time in each thermal impact group
    foreach(i = 1:(nrow(ExcCrit)),.combine='rbind') %do% {
      if(is.na(ExcCrit$TemperatureMax_degC[i])){
        prcExc <- length(which( Tmean < ExcCrit$TemperatureMin_degC[i])) /  length(Tmean)
      }else{
        if(is.na(ExcCrit$TemperatureMin_degC[i])){
          prcExc <- length(which(Tmean >= ExcCrit$TemperatureMax_degC[i])) /  length(Tmean)
        }else{
          prcExc <- length(which(Tmean > ExcCrit$TemperatureMin_degC[i] &
                                   Tmean <= ExcCrit$TemperatureMax_degC[i])) /  length(Tmean)
        }
      }
      return(data.frame(c(ExcCrit[i,c(1:4)],Prc = round(prcExc*100))))
    }
  }
  SumExcSimple <- function(Tmean,Thresh){ #summarize percent time below Threshold
    DysBlwThrsh <- length(which( Tmean <= Thresh))
    prcExc <- length(which( Tmean <= Thresh)) /  length(Tmean)
    return(data.frame(LifeStage=c('DaysBelowThresh','PrcDaysBelowThresh'),
           value = c(DysBlwThrsh,prcExc*100)))
  }

  SumExcSimplePrc <- function(Tmean){ #summarize percent time below 20 deg C
    prcExc <- length(which( Tmean <= 18)) /  length(Tmean)
    return(data.frame(LifeStage='BelowThresh',Prc = round(prcExc*100)))
  }
  
  if(any(alt != 'Measured')){
    print(paste('Summarizing Biological Threshold Exceedences'))
    if(length(fls7d)>0){
      # River models have been run
      ExcSmry <- foreach(fl = (1:length(fls7d)),.combine='rbind') %do% {
        x7t <- read_csv(Flfls7d[fl],skip=2,show_col_types = F)
        if(any(grepl('\\/',x7t$Date))){
          x7t$Date <- as.Date(x7t$Date,'%m/%d/%Y')
        }
        x7t <- x7t %>% mutate(Year = format(Date,'%Y'),
                              Month = format(Date,'%b'),Day = format(Date,'%d'),
                              Will_RM.midpt = round(Will_RM.midpt,2),
                              River = as.factor(gsub(' abv Lookout Point Lake','',River))#,
                              #Thresh = ifelse(grepl('82|119.32',Will_RM.midpt),24,18)
                              #Thresh = 18
                              ) %>%
                       # Add filter here to gage and RM designation
                       filter(Will_RM.midpt %in% EIS_outputlocs$`Will_RM-midpt` &
                                River %in% EIS_outputlocs$River)
       if(nrow(x7t) >0 ){
          print(paste(fls7d[fl],';',fl,'Of',length(fls7d)))
          ExcPrc1 <- x7t %>%
            group_by(Will_RM.midpt,River) %>%
            dplyr::summarize(SumExcSimple(Tmax,18))  %>%
            mutate(Year = strsplit(fls7d[fl],'_')[[1]][3],
                   LifeStage = gsub('DaysBelowThresh','DaysBelow18C',LifeStage))
          ExcPrc2 <- x7t %>%
            filter(grepl('Oct|Nov|Dec',Month)) %>%
            group_by(Will_RM.midpt,River) %>%
            dplyr::summarize(SumExcSimple(Tmax,10))  %>%
            mutate(Year = strsplit(fls7d[fl],'_')[[1]][3],
                   LifeStage = gsub('DaysBelowThresh','DaysBelow10C',LifeStage))
          ExcPrc <- bind_rows(ExcPrc1,ExcPrc2) %>%
            filter(!is.na(LifeStage))
        }else{
          ExcPrc <- NULL
        }
        return(ExcPrc)
      }
      print('Finished Reading ')
      ExcSmry <- ExcSmry %>%
        dplyr::rename( `Will_RM-midpt` = `Will_RM.midpt`) %>%
        inner_join(EIS_outputlocs,by = c("Will_RM-midpt","River")) %>% #
        dplyr::rename(Location = 'USACE_ID') %>%
        ungroup() %>%
        filter(Location %in% StsCWMSall) %>%
        mutate(Location = factor(Location,
                                 levels = StsCWMSall,#[EIS_outputlocs$`Gage Name` %in% Location],
                                 ordered=T)) %>%
        select(c(Location,LifeStage,value,Year)) %>% #-c(Date.Range,Gage,Model,River,crit.op,criteria,))
        mutate(LifeStage = factor(LifeStage,levels = unique(LifeStage),ordered=T))
      print('Finished Merging with location info')
    }else{
      # No river models have been run
      Tdams <- Tdams %>%
        mutate(Month = format(as.Date(JDAY,origin=as.Date('2010-12-31')),'%b'),
               Day = format(as.Date(JDAY,origin=as.Date('2010-12-31')),'%d')) 
      # Extract 7dadm from data already retrieved below dams
      ExcPrc1 <- Tdams %>%
        select(Year,Location,Alt,T7dadm) %>%
        group_by(Location,Alt,Year) %>%
        dplyr::summarize(SumExcSimple(T7dadm,18))  %>%
        mutate(LifeStage = gsub('DaysBelowThresh','DaysBelow18C',LifeStage))
      
      # Left off here!!!
      ExcPrc2 <- Tdams %>%
        filter(grepl('Oct|Nov|Dec',Month)) %>%
        group_by(Location,Alt,Year) %>%
        dplyr::summarize(SumExcSimple(T7dadm,10))  %>%
        mutate(LifeStage = gsub('DaysBelowThresh','DaysBelow10C',LifeStage))
      ExcSmry <- bind_rows(ExcPrc1,ExcPrc2) %>%
        filter(!is.na(LifeStage))
    }
          
  }else{ # Proceed here if measured data is read in
    # Reformat measured temperatures to merge with alternative data
    TmeasDmax <- Tmeas %>% mutate(Year = as.factor(Year),
                                  Location = as.factor(Site)) %>%
      group_by(Location,Date,Year) %>%
      dplyr::summarize(Dmax = max(Tmeas))
    Tmeas7dadm <- foreach(y = levels(TmeasDmax$Year),.combine = 'rbind') %do%{
      foreach(l = levels(TmeasDmax$Location),.combine = 'rbind') %do%{
        TmDmaxy <- TmeasDmax %>% filter(Location == l, Year == y) %>%
          mutate(JDAY = julian(Date,origin = as.Date(paste0(as.numeric(y)-1,'-12-31'))))
        TmDmy <- madm7.oncols(TmDmaxy[,c('JDAY','Dmax')])
        TmDmaxy <- merge(TmDmaxy %>% select(-'Dmax'),TmDmy,by='JDAY')
        TmDmaxy$Location <- l
        return(TmDmaxy)
      }
    }

    Tmeas7dadm <- Tmeas7dadm %>% mutate(Location = as.factor(Location))
    Tmeas7dadm1 <- Tmeas7dadm %>%
      group_by(Location,Year) %>%
      dplyr::summarize(SumExcSimple(Dmax,c2f(18))) %>%
      mutate(LifeStage = gsub('DaysBelowThresh','DaysBelow18C',LifeStage))
    Tmeas7dadm2 <- Tmeas7dadm %>%
      filter(grepl('Oct|Nov|Dec',format(Date,'%b'))) %>%
      group_by(Location,Year) %>%
      dplyr::summarize(SumExcSimple(Dmax,c2f(10))) %>%
      mutate(LifeStage = gsub('DaysBelowThresh','DaysBelow10C',LifeStage))
    ExcSmry <- bind_rows(Tmeas7dadm1,Tmeas7dadm2) %>%
      filter(!is.na(LifeStage),
             Location %in% StsCWMSall) %>%
      mutate(LifeStage = factor(LifeStage,levels = unique(LifeStage),ordered=T),
             Location = factor(Location,
                               levels = StsCWMSall,#[EIS_outputlocs$`Gage Name` %in% Location],
                               ordered=T),
             Year = as.character(Year))

  } #End Else for measured data

  print('Averaging over 3 years')
  # Average over all 3 simulation years
  ExcSmryAvg <- ExcSmry %>% group_by(Location,LifeStage,Alt) %>%
    dplyr::summarize(value = mean(value,na.rm=T))%>%
    mutate(Year = '3YrAvg') %>%
    ungroup()
  ExcSmry <- ExcSmry  %>% 
    mutate(Year = as.character(Year)) %>%
    bind_rows(ExcSmryAvg) %>% 
    mutate(Year = factor(Year,levels=c('2011','2015','2016','3YrAvg'),ordered=T)) 
  print('Finished Averaging over 3 years')
  
  return(ExcSmry)
}


ReadW2Mo <- function(fldr,pattern = "degF.csv"){ #pattern = "diff-from-Baseline.csv"
  # Summarize monthly averages
  flsMo <- list.files(file.path(fldr,'river_stats'),pattern = pattern)
  keepMo <- grepl('Monthly',flsMo) #& grepl('degF',flsMo)
  FlflsMo <- list.files(file.path(fldr,'river_stats'),full.names = T,pattern = pattern)
  Alt <- names(fldr)

  if(Alt != 'Measured'){
    MoAvgSmry <- foreach(fl = (1:length(flsMo))[keepMo],.combine='rbind') %do% {
      Mot <- read_csv(FlflsMo[fl],skip=2,show_col_types = F)
      if(pattern == "degF_diff-from-Baseline.csv"){
        stt = rev(strsplit(flsMo[fl],'_')[[1]])[3]
      }else{
        stt = rev(strsplit(flsMo[fl],'_')[[1]])[2]
      }
      Mot <- Mot %>%
        select(-`Apr-Oct`) %>%
        inner_join(EIS_outputlocs %>% dplyr::rename(Location = `Gage Name`) %>%
                     select(-River),by = 'Location') %>%
        select(-c('Location',`Will_RM-midpt`,'Gage','Model')) %>%
        mutate(Year = as.factor(Year),
               River = as.factor(River),
               Location = factor(USACE_ID,
                                 levels = StsCWMSall,#[EIS_outputlocs$`Gage Name` %in% Location],
                                 ordered=T)) %>%
               #Location = factor(USACE_ID,levels = EIS_outputlocs$`USACE_ID`)) %>%
        select(-USACE_ID,-River) %>%
        pivot_longer(cols = -c(Location,Year),names_to = 'Month') %>%
        mutate(Alt = Alt,
               Stat = stt,
               Month = factor(substr(Month,1,3),levels=month.abb[-c(1:3,11,12)],ordered=T)) %>%
        filter(!is.na(Location))
      return(Mot)
    }

   }else{
    # Reformat measured temperatures to merge with alternative data
    MoAvgSmry <-
      Tmeas %>% mutate(Year = as.factor(Year),
                       Location = factor(Site,levels = StsCWMSall,ordered=T)) %>%
      filter(!is.na(Location))      %>%
      group_by(Location,Year,Date) %>%
      dplyr::summarize(value = mean(Tmeas,na.rm=T)) %>%
      mutate(Month = factor(format(Date,'%b'),levels = month.abb[-c(1:3,11,12)])) %>%
      group_by(Location,Year,Month) %>%
      dplyr::summarize(value = max(value,na.rm=T)) %>%
      mutate(Stat = 'MonthlyMax') %>%
      bind_rows(Tmeas %>% mutate(Year = as.factor(Year),
                                 Location = factor(Site,levels = StsCWMSall,ordered=T),
                                 Month = factor(format(`Date Time`,'%b'),levels = month.abb[-c(1:3,11,12)])) %>%
                  filter(!is.na(Location))      %>%
                  group_by(Location,Year,Date) %>%
                  dplyr::summarize(value = mean(Tmeas,na.rm=T)) %>%
                  mutate(Month = factor(format(Date,'%b'),levels = month.abb[-c(1:3,11,12)])) %>%
                  group_by(Location,Year,Month) %>%
                  dplyr::summarize(value = mean(value)) %>%
                  mutate(Stat = 'MonthlyMean') ) %>%
      mutate(Alt = 'Measured',
             Month = factor(Month,levels = month.abb[-c(1:3,11,12)],ordered=T))
  }
  # Average over all 3 simulation years
  MoAvgSmryAvg <- MoAvgSmry  %>% group_by(Location,Month,Stat) %>%
    dplyr::summarize(value = mean(value,na.rm=T))%>%
    mutate(Year = 'Average',
           Alt = Alt) %>%
    ungroup()
  MoAvgSmry <- MoAvgSmry  %>%
    bind_rows(MoAvgSmryAvg) %>%
    mutate(Year = as.factor(Year),
           Alt = Alt,
           Stat = as.factor(Stat))
  return(MoAvgSmry)
}

ReadAltW2Res <- function(fldrs){
  ##### Calculate emergence timing ##############
  noMissingAlts <- !sapply(fldrs,is.null) #| !sapply(fldrs,is.na)
  noMissingAltsBaseline <- !grepl('Baseline',names(fldrs)) & noMissingAlts
  Tdams <- foreach(f = which(noMissingAlts),.combine='rbind') %do%{
    ReadW2Tdam(fldr = fldrs[f])
  }
  Tdams <- Tdams %>%
    mutate(Alt = factor(Alt),Year = factor(Year),
           Location = factor(Location,levels = Sts3ltr,ordered=T))
    
  #~~~~~~~~~~~~~~~~~~~
  # Calculate estimated emergence
  Em <-SumEmerge(Tdams = Tdams) 
  Em <- Em %>% mutate(Year = factor(Year),SpawnDay = factor(SpawnDay))
  
  EmDif <- Em %>% filter(!is.na(Location),SpawnDay == 'Average') %>% #!is.na(nmiss),
    select(-c('atu','SpawnDay')) %>% #,'nmiss'
    pivot_wider(id_cols =  c('Location','Year'),names_from = 'Alt',
                values_from = c('atu.d')) %>%
    ungroup()
  EmDif2 <- foreach(a = names(noMissingAlts)[noMissingAltsBaseline],
                    .combine='cbind') %do%{
    EmDif %>% select(a) - EmDif %>% select(Baseline)
  }
  colnames(EmDif2) <- paste0(names(noMissingAlts)[noMissingAltsBaseline],'_Dif')
  EmDif <- cbind(EmDif,EmDif2)
  rm(EmDif2)
  EmDif <- EmDif %>%
    pivot_longer(cols = -c("Location","Year"), #"SpawnDay",
                                    names_to = 'Alt') %>%
    mutate(Alt = factor(Alt))
  EmDifSDAvg <- EmDif %>%
    filter(grepl('Dif',Alt)) %>% #,Year != 'Average'
    group_by(Location,Year,Alt) %>% #Year
    dplyr::summarize(Avg = round(mean(value))) %>%
    mutate(Alt = factor(gsub('_Dif','',as.character(Alt))))

  EmDifSDYrAvg <- EmDif %>%
    filter(grepl('Dif',Alt)) %>% #,Year != 'Average'
    group_by(Location,Alt) %>% #Year
    dplyr::summarize(Avg = round(mean(value))) %>%
    mutate(Alt = factor(gsub('_Dif','',as.character(Alt))),
           Year = '3YrAvg')
  EmDifSDYrAvg <- bind_rows(EmDifSDAvg,EmDifSDYrAvg) %>%
    mutate(Year = factor(Year,levels=c('2011','2015','2016','3YrAvg'),ordered=T))

  #~~~~~~~~~~~~~~~~~~~
  ##### Get Monthly averages ##############
  if(any(grepl('ALB|SLM',Sts3ltr))){
    MoAvg <- foreach(f = which(noMissingAlts),.combine='rbind') %do%{
      ReadW2Mo(fldr = fldrs[f],pattern = "degF.csv")
    }
  }else{
    MoAvg <- Tdams %>% 
      mutate(Month = format(as.Date(JDAY,origin = as.Date('2000-01-01')),'%b')) %>%
      group_by(Location, Year,Month,Alt) %>%
      dplyr::summarise(MonthlyMax = max(c2f(TMax)),
                       MonthlyMin = max(c2f(TMin)),
                       MonthlyMean = max(c2f(TMean))) %>%
      pivot_longer(cols = -c('Location', 'Year','Month','Alt'),
                   names_to = 'Stat') %>%
      mutate(Stat = as.factor(Stat),Month = factor(Month,levels = month.abb,ordered=T))
    
    # Average over all 3 simulation years
    MoAvgSmry <- MoAvg  %>% 
      group_by(Location,Month,Stat,Alt) %>%
      dplyr::summarize(value = mean(value,na.rm=T))%>%
      mutate(Year = '3YrAvg') %>%
      ungroup()
    MoAvg <- MoAvg  %>%
      bind_rows(MoAvgSmry) %>%
      mutate(Year = as.factor(Year),
             Alt = Alt,
             Stat = as.factor(Stat))
    
  }
  MoAvg <- MoAvg %>% mutate(Alt = as.factor(Alt),
                            Year = factor(gsub('Average','3YrAvg',Year),
                                          levels=c('2011','2015','2016','3YrAvg'),ordered=T))

  if(!any(grepl('Measured',names(altDirs)))){
    # For all scenarios that are not "Measured"
    if(!any(grepl('ALB|SLM',Sts3ltr))){
      MoAvgDif <- MoAvg %>% 
        pivot_wider(names_from = 'Alt',
                    values_from = c('value')) %>%
        ungroup()
      MoAvgDif2 <- foreach(a = names(noMissingAlts)[noMissingAltsBaseline],
                           .combine='cbind') %do%{
                             MoAvgDif %>% select(a) - MoAvgDif %>% select(Baseline)
                           }
      colnames(MoAvgDif2) <- paste0(names(noMissingAlts)[noMissingAltsBaseline],'_Dif')
      MoAvgDif <- cbind(MoAvgDif,MoAvgDif2)
      rm(MoAvgDif2)
      MoAvgDif <- MoAvgDif %>%
        pivot_longer(cols = -c("Location","Year","Month","Stat"), #"SpawnDay",
                     names_to = 'Alt') %>%
        mutate(Alt = factor(Alt))
      
      MoAvgDifSDAvg <- MoAvgDif %>%
        filter(grepl('Dif',Alt)) %>% #,Year != 'Average'
        group_by(Location,Year,Alt,Month,Stat) %>% #Year
        dplyr::summarize(Avg = round(mean(value))) %>%
        mutate(Alt = factor(gsub('_Dif','',as.character(Alt))))
      
      MoAvgDifSDYrAvg <- MoAvgDif %>%
        filter(grepl('Dif',Alt)) %>% #,Year != 'Average'
        group_by(Location,Alt,Month,Stat) %>% #Year
        dplyr::summarize(Avg = round(mean(value))) %>%
        mutate(Alt = factor(gsub('_Dif','',as.character(Alt))),
               Year = '3YrAvg')
      MoAvgDifSDYrAvg <- bind_rows(MoAvgDifSDAvg,MoAvgDifSDYrAvg) %>%
        mutate(Year = factor(Year,levels=c('2011','2015','2016','3YrAvg'),ordered=T))
      
    }else{
      MoAvgDif <- foreach(f = which(noMissingAltsBaseline),.combine='rbind') %do%{
        if(!grepl('Baseline',fldrs[f])){
          ReadW2Mo(fldr = fldrs[f],pattern = "degF_diff-from-Baseline.csv")
        }
      }
    } # End if/else for river model output
  }else{ 
    # Process all Measured data
    MoAvgDif <- MoAvg %>% filter(!is.na(Location) & 
                                   !is.na(Month) &
                                   Stat == 'MonthlyMean') %>%
      ungroup() %>%
      pivot_wider(id_cols = c('Location','Year','Month','Stat'),
                  names_from = 'Alt',values_from = 'value') %>%
      mutate(Measured = unlist(Measured),
             Baseline = unlist(Baseline)) %>%
      mutate(value = unlist(Measured) - unlist(Baseline),Alt = "Measured") %>%
      select(Location,Year,Month,Stat,value,Alt)
    
  }
  MoAvgDif <- MoAvgDif %>%
    mutate(Alt = as.factor(Alt),
      Year = factor(gsub('Average','3YrAvg',Year),
        levels=c('2011','2015','2016','3YrAvg'),ordered=T)) %>%
    filter(!is.na(Month))

  #~~~~~~~~~~~~~~~~~~~
  # ##### Get percent time within threshold temps ##############
  # For all scenarios that are not "Measured"
  if(!any(grepl('ALB|SLM',Sts3ltr))){
    ExcDif <- ReadW2Tdavg(fldr = fldrs,Tdams = Tdams)
  }else{
    Exc <- foreach(f = which(noMissingAlts),.combine='rbind') %do%{
      print(paste('~~Reading T file f = ',f))
      x <- ReadW2Tdavg(fldr = fldrs[f])
      print(paste('~~Finished Reading T file f = ',f))
      return(x)
    }
    print(str(Exc))
    Exc <- Exc %>% mutate(Alt = as.factor(Alt))
    #,Prc = round(Prc))
    ExcDif <- Exc %>%
      pivot_wider(names_from = Alt,
                  #id_cols =c('Alt','Year','Location','LifeStage'),
                  values_from = value)
    ExcDif2 <- foreach(a = names(fldrs)[noMissingAltsBaseline],.combine='cbind') %do%{
      ExcDif[,match(a,colnames(ExcDif))] - ExcDif[,match('Baseline',colnames(ExcDif))]
    }
    colnames(ExcDif2) <- paste0(colnames(ExcDif2),'_Dif')
    ExcDif <- cbind(ExcDif,ExcDif2)
    rm(ExcDif2)
    ExcDif <- ExcDif %>% pivot_longer(cols = -c("Location","LifeStage","Year"),
                                      names_to = 'Alt') %>%
      filter(grepl('Dif',Alt)) %>% #,Year != 'Average'
      mutate(Alt = factor(gsub('_Dif','',as.character(Alt))),
             Year = factor(gsub('Average','3YrAvg',Year),
                           levels=c('2011','2015','2016','3YrAvg'),ordered=T)) %>%
      filter(grepl(Sts2Smrz,Location))
    
  } #End if/else for river model output
  

  #~~~~~~~~~~~~~~~~~~~
  ##### Calculate spill percentage ##############
  
  if(!any(grepl('Measured',names(altDirs)))){
    Qsp <- foreach(f = which(noMissingAlts),.combine='rbind') %do%{
      ReadW2Qdam(fldr = fldrs[f])
    }
    Qsp <- Qsp %>% mutate(Alt = as.factor(Alt))
    QspDif <- Qsp %>%
      pivot_wider(names_from = Alt,#c('Location','Alt','SpawnDay','Year'),
                  values_from = c('PrcSpl')) %>%
      ungroup()
    # Calculate the difference between Baseline and each alternative
    QspDif2 <- foreach(a = names(noMissingAlts)[noMissingAltsBaseline],
                       .combine='cbind') %do%{
                         round(QspDif %>% select(a) - QspDif %>% select(Baseline))
                       }
    colnames(QspDif2) <- paste0(colnames(QspDif2),'_Dif')
    QspDif <- cbind(QspDif,QspDif2)
    rm(QspDif2)
    QspDif <- QspDif %>%
      pivot_longer(cols = -c("Location","Month"),
                   names_to = 'Alt') %>%
      mutate(Alt = factor(Alt))

    # Read and calculate difference from temp targets
    # TtargDifLocs <- foreach(f = which(noMissingAlts),.combine='rbind') %do%{
    #   ReadW2TtDiff(fldr = fldrs[f])
    # }
    TtargDifLocs <- CalcW2TtDiff(Tdams = Tdams)
    TtargDifLocs <- TtargDifLocs %>% mutate(Alt = as.factor(Alt),
                                            TargNm = as.factor(TargNm))
    
    list2env(list(ExcDif = ExcDif,#Exc = Exc,
                  MoAvg = MoAvg,MoAvgDif = MoAvgDif,
                  Em = Em, EmDif = EmDif,EmDifSDYrAvg = EmDifSDYrAvg,
                  QspDif = QspDif,
                  TtargDifLocs = TtargDifLocs),
             envir = .GlobalEnv)
   }else{
     list2env(list(Exc = Exc,ExcDif = ExcDif,
                   MoAvg = MoAvg,MoAvgDif = MoAvgDif,
                   Em = Em, EmDif = EmDif,EmDifSDYrAvg = EmDifSDYrAvg),
              envir = .GlobalEnv)
   }
} #End ReadAltW2  

CalcTTdifs <- function(){
    
  th<- 2# Threshold temp [deg F] for "off" target

  # Summarize a comparison against temperature targets of choice
  TtTime <- TtargDifLocs %>%
    # mutate(LifeStage = ifelse(grepl('Jun|Jul|Aug',Month),'JunAug', #'Apr|May|Jun|Jul|Aug',
    #                         ifelse(grepl('Sep|Oct|Nov',Month),'SepNov','DecMay'))) %>%
    mutate(LifeStage = ifelse(grepl('Apr|May|Jun|Jul|Aug',Month),'AprAug','SepMar')) %>%
    #filter(!grepl('DecMay',LifeStage)) %>%
    mutate(LifeStage = as.factor(LifeStage)) %>%
    group_by(LifeStage,Year,Alt,Location,TargNm) %>%
    dplyr::summarize(
      TDiffFromTarg = mean(Diff),
      #DaysOffTarg = length(which(Diff>=th)),# | Diff<=(-th))),
      DaysNearTarg = length(which(abs(Diff)<=th)))
  #PrcntTimeOffTarg = round(100*(DaysOffTarg/length(Diff))),
  #PrcntTimeNearTarg = round(100*(DaysNearTarg/length(Diff)))
  
  # Tabulate Difference from Baseline
  TtTimeDif <- foreach(a = levels(TtTime$Alt)[!grepl('Baseline',levels(TtTime$Alt))],.combine='rbind') %do%{
    TtTime %>% filter(Alt == a) %>%
      dplyr::rename(DaysNearTargAlt = DaysNearTarg,
             TDiffFromTargAlt = TDiffFromTarg) %>%
      ungroup() %>%
      #select(Year,LifeStage,Location,DaysNearTargAlt) %>%
      select(-Alt) %>%
      full_join(TtTime %>% filter(Alt == 'Baseline') %>%
                  dplyr::rename(DaysNearTargBaseline = DaysNearTarg,
                         TDiffFromTargBaseline = TDiffFromTarg) %>%
                  ungroup() %>%
                  #select(Year,LifeStage,Location,DaysNearTargAlt) %>%
                  select(-Alt)) %>%
      mutate(DaysNearTargBaselineDif = DaysNearTargAlt -  DaysNearTargBaseline,
             TDiffFromTargBaselineDif = TDiffFromTargAlt - TDiffFromTargBaseline,
             Alt = a) %>%
      select(-DaysNearTargAlt,-DaysNearTargBaseline,-TDiffFromTargBaseline,-TDiffFromTargAlt)
  }
  
  TtTimeDif <- TtTimeDif %>%
    bind_rows(TtTimeDif %>% group_by(LifeStage,Location,Alt,TargNm) %>%
                dplyr::summarize(DaysNearTargBaselineDif = round(mean(DaysNearTargBaselineDif)),
                                 TDiffFromTargBaselineDif = round(mean(TDiffFromTargBaselineDif))) %>%
                mutate(Year = '3YrAvg'))
  TtTimeDif <- TtTimeDif %>%
    mutate(Alt = as.factor(Alt),#,levels = names(altDirs),ordered=T),
           Location = factor(Location,levels=rev(Sts3ltr),ordered=T), #"GPR",
           Year = factor(Year,ordered=T),
           #Month = factor(Month,levels = month.abb,ordered=T)
    )
  
  TtDifYr <- TtTimeDif %>% group_by(Location,Alt,LifeStage,Year,TargNm) %>%
    dplyr::summarize(value = mean(DaysNearTargBaselineDif)) %>%
    # mutate(Location = factor(gsub('HCR','HCRO',gsub('DEX','DEXO',gsub('CGR','CGRO',gsub('FOS','SSFO',gsub('GPR','GPRO',gsub('BCL','BCLO',Location)))))),
    #                          levels = rev(StsCWMSall),ordered=T), #,"GPRO"
    #) %>%
    ungroup()
  
  TtTime <- TtTime %>%
    #select(-DaysNearTarg) %>%
    bind_rows(TtTime %>% #select(-DaysNearTarg) %>%
                group_by(LifeStage,Location,Alt,TargNm) %>%
                dplyr::summarize(TDiffFromTarg = mean(TDiffFromTarg)) %>%
                mutate(Year = '3YrAvg')) %>%
    #group_by(Location,Alt,LifeStage,Year) %>%
    mutate(Alt = factor(Alt,levels = names(altDirs),ordered=T),
           Location = factor(Location,levels=rev(Sts3ltr),ordered=T), #"GPR",
           Year = as.factor(Year)
           #LifeStage = as.factor(gsub('DaysNearTarg','',as.character(LifeStage)))
           #Month = factor(Month,levels = month.abb,ordered=T)
    )
  
  list2env(list(TtTime = TtTime,
                TtTimeDif = TtTimeDif,
                TtDifYr = TtDifYr),
           envir = .GlobalEnv)
}

MakeTimpacts <- function(ttType){
  if(!any(grepl('Measured',names(altDirs)))){
    Data4Impacts <-
      ExcDif %>% mutate(LifeStage = as.character(LifeStage),
                        Location = as.character(Location)) %>%
      full_join(TtDifYr %>%
                  filter(TargNm == ttType) %>%
                  mutate(LifeStage = gsub('SepMar','DaysNearTargSepMar',
                         gsub('AprAug','DaysNearTargAprAug',LifeStage))) %>%
                  select(-TargNm)) %>%
      #full_join(TMDLTtTimeDif) %>%
      #full_join(EIS_outputlocs %>% rename(Location = USACE_ID) %>%
      #            select(Location,River),by='Location') %>%
      #filter(River %in% c("McKenzie" ,"Middle Fork Willamette", "North Santiam" ,
      #                    "South Santiam" ,"Willamette")) %>%
      # filter(grepl(Sts2Smrz,Location)) %>%
      mutate(#River = factor(River),
             Location = factor(Location,levels = Sts3ltr), #"GPRO",
             LifeStage = as.factor(LifeStage)) %>%
      filter(!is.na(value))
    
    
    Timpacts <- DefineTempImpacts(Data4Impacts,TThresh)
    
    list2env(list(Timpacts = Timpacts),
             envir = .GlobalEnv)
  }
} # End MakeTimpacts

TThresh <- data.frame(
  # TMDLTtargExcVal1 = 4, #PrcntTimeNearTargDif
  # TMDLTtargExcVal2 = 9,
  # TMDLTtargExcVal3 = 19,
  TtargExcVal1 = 9, #PrcntTimeNearTargDif
  TtargExcVal2 = 19,
  TtargExcVal3 = 49,
  ExcVal18DegC1 = 4, #'DaysBelow18C',
  ExcVal18DegC2 = 9,
  ExcVal18DegC3 = 14
  # ExcVal10DegC1 = 4, #'DaysBelow10C',
  # ExcVal10DegC2 = 9,
  # ExcVal10DegC3 = 14,
  # EmrgncVal1 = 4, #EggEmergenceDifAvg
  # EmrgncVal2 = 9,
  # EmrgncVal3 = 14
  )

DefineTempImpacts <- function(Data4Impacts,TThresh){

  Impacts <- c('Negligible','Minor','Moderate','Major')
  # Check Differences from Baseline in each category and define impact
    # Is threshold for Cat1 exceeded?
  I1 <- function(x,thresh){
    exm <- which.max(unlist(abs(x)))
    if(any(abs(x) > thresh[1]) ){
      if(any(abs(x) > thresh[3] ) ){
        i=Impacts[4]
      }else{
        if(any(abs(x) > thresh[2]) ){
          i=Impacts[3]
        }else{
          i=Impacts[2]
        }
      }
    }else{
      i=Impacts[1]
    }
    if(i !='Negligible'){
      if(any(x[exm] > 0)){ # A positive change is a delay in emergence
        i = paste(i,'Ben')
      }else{
        i = paste(i,'Adv')
      }
    }
    return(i)
  }
  
  TimpactsByLoc <- foreach(Loc = levels(Data4Impacts$Location),.combine = 'rbind') %do%{
    dr <- Data4Impacts %>% filter(Location == Loc) 
    if(nrow(dr)>0){
      # Left off here!
      # Define Impacts
      dr1 <- dr %>%
        filter(grepl('3YrAvg',Year),
               !grepl('PrcDay',LifeStage),
               grepl('DaysNearTarg|DaysBelow18',LifeStage)) %>%
        select(LifeStage,Alt,value) %>%
        pivot_wider(names_from = LifeStage,values_from = 'value')
      if(!any(grepl('DaysNearTargAprAug',colnames(dr1)))){
        dr1$DaysNearTargAprAug = NA
      }
      if(!any(grepl('DaysNearTargSepMar',colnames(dr1)))){
        dr1$DaysNearTargSepMar = NA
      }
      dr1 <- dr1 %>% ungroup() %>% select(-Year) 
      dri <- foreach(a = levels(dr1$Alt)[!grepl('Baseline',levels(dr1$Alt))],
                     .combine = 'rbind') %do% {
        dr1a <- dr1 %>% filter(grepl(a,Alt))
        dr1a <- dr1a %>%
          mutate(
            # Time near target into seasons
            DaysNearTargAprAug = ifelse(is.na(dr1a$DaysNearTargAprAug),NA,
                                        I1(x = dr1a$DaysNearTargAprAug,
                                           TThresh[,grepl('Ttarg',colnames(TThresh))])),
            DaysNearTargSepMar = ifelse(is.na(dr1a$DaysNearTargSepMar),NA,
                                        I1(x = dr1a$DaysNearTargSepMar,
                                           TThresh[,grepl('Ttarg',colnames(TThresh))])),
            DaysBelow18C = I1(x = dr1a$DaysBelow18C,
                              TThresh[,grepl('ExcVal18DegC',colnames(TThresh))])
            
          )
         return( dr1a %>% #mutate(Location = Loc) %>%
                 pivot_longer(cols = -c('Alt','Location'),
                              names_to = 'Season',values_to = 'Impact'))
      } # End Loop by Alt
      return(dri)
    }#End else statement for rows with data
  }# End loop by location
  
  TimpactsByLoc <- TimpactsByLoc %>% 
    mutate(Alt = factor(Alt,ordered = T,levels = names(altDirs)),
           Location = factor(Location,ordered=T,levels = levels(Data4Impacts$Location)),
           Season = factor(#gsub('PrcntTimeNearTarg','PrcntTimeNearTarg (All Year)',
                        gsub('DaysBelow18C','DaysBelow18C (Summer)',
                             #gsub('DaysBelow10C','DaysBelow10C (Autumn)',
                             #gsub('EggEmergence','EggEmergence (Autumn)',
                             Season),
                        levels = c('DaysBelow18C (Summer)',#'DaysBelow10C (Autumn)','EggEmergence (Autumn)',
                                   #'DaysNearTMDLTargJunAug','DaysNearTMDLTargSepNov',
                                   'DaysNearTargAprAug',
                                   'DaysNearTargSepMar'),ordered=T), #,'PrcntTimeNearTarg (All Year)'
           Impact = as.factor(Impact))
  return(TimpactsByLoc)
}

ComparePlotAltW2 <- function(ttType = 'W2'){
  # ttType is the temperature target source ('W2', 'RA', or 'TMDL')
  pltNm <- paste0('TempComp_',ttType,gsub(', ','_',toString(names(altDirs))),'.png')
  
  library(ggplot2)
  if(length(names(altDirs))>2){
    # Table with annual average time near temp target
    DaysOnTtTbl <- ggplot(TtDifYr %>% filter(Alt %in% names(altDirs),
                                             TargNm == ttType),
                          aes(x=Year,y=Location,
                                fill=value,
                                label=round(value))) +
      geom_tile(alpha=0.8) +
      geom_text(color='black',size=4) +
      facet_grid(LifeStage~Alt,scales = 'free') +
      scale_fill_gradient2(low = "orange",mid = "grey",  high = "darkblue",midpoint = 0) +
      ggtitle(paste0('Difference From Baseline in Annual Average Number of Days\n Within 2 degrees F of ',
                     ttType,' Temperature Target') ) +
      theme(axis.title.x=element_blank(),
            strip.text.y = element_text(angle = 0),
            legend.position ='none',
            axis.text.x=element_text(angle=45,hjust=1))
    ggsave(plot=DaysOnTtTbl,device = 'png',width=10,height=5,
           filename=file.path(compDir,gsub('Comp',paste0('DaysOn',ttType,'TtargSumDifTbl'),pltNm)))
    
    # Table with annual average difference from temp target
    TtDifTbl <- ggplot(TtTime %>% filter(Alt %in% names(altDirs),
                                         TargNm == ttType),
                          aes(x=Year,y=Location,
                              fill=TDiffFromTarg,
                              label=round(TDiffFromTarg))) +
      geom_tile(alpha=0.8) +
      geom_text(color='black',size=4) +
      facet_grid(LifeStage~Alt,scales = 'free') +
      scale_fill_gradient2(high = "orange",mid = "grey",  low = "darkblue",midpoint = 0) +
      ggtitle(paste0('Seasonal Average Difference From ',ttType,'Temperature Target (degrees F)')) + 
      theme(axis.title.x=element_blank(),
            strip.text.y = element_text(angle = 0),
            legend.position ='none',
            axis.text.x=element_text(angle=45,hjust=1))
    ggsave(plot=TtDifTbl,device = 'png',width=10,height=5,
           filename=file.path(compDir,gsub('Comp',paste0('SsnlDiffFrom',ttType,'TtargSumTbl'),pltNm)))
    
  }

  if(!any(grepl('Measured',names(altDirs)))){

    # Plot the Impacts from temperature in each Alternative
    TImpctTbl <- ggplot(Timpacts,
                        aes(y=Location,x=Alt,fill=Impact,
                            label=gsub('Major','Maj',
                                       gsub('Minor','Min',
                                            gsub('Moderate','Mod',Impact))))) +
      geom_tile() +
      geom_text(color='white',size=2) +
      scale_fill_manual(values = ImpactPal) +
      scale_y_discrete(limits=rev) +
      facet_grid(.~Season,scales = 'free') +
      ggtitle('Temperature Impacts Based on\nNumber of Days Below 18DegC (64.4DegF) (Summer)\nDifference from Baseline in Number of Days within 2 degrees F of Temperature Targets') +
      theme(axis.title.x=element_blank(),
            strip.text.y = element_text(angle = 0),
            axis.text.x=element_text(angle=45,hjust=1))
    ggsave(plot=TImpctTbl,device = 'png',width=10,height=5,
           filename=file.path(compDir,gsub('Comp','Impacts',pltNm)))
  }

  if(length(names(altDirs))>2){
    SmmrThrshDifOptTblAvg <- ggplot(ExcDif %>%
                                filter(Alt %in% names(altDirs),
                                       grepl('DaysBelow18C',LifeStage),
                                       !grepl('Prc',LifeStage)),
                                aes(x=Year, y=Location, fill=value,label=round(value))) +
      geom_tile(alpha=0.8) +
      geom_text(color='black',size=4) +
      facet_grid(.~Alt,scales = 'free') +
      scale_y_discrete(limits=rev) +
      scale_fill_gradient2(low = "orange",mid = "grey",  high = "darkblue",midpoint = 0) +
      ggtitle('Difference from Baseline in Days Below 18 Degrees C Each Year') +
      theme(axis.title.x=element_blank(),
            strip.text.y = element_text(angle = 0),
            legend.position ='none',
            axis.text.x=element_text(angle=45,hjust=1))
      ggsave(plot=SmmrThrshDifOptTblAvg,device = 'png',width=10,height=5,
           filename=file.path(compDir,gsub('Comp','DysBlwSmmrThrshDifTblAvg',pltNm)))

  spawndaysMD <- format(as.Date(spawndays,origin = as.Date('2000-01-01')),'%m/%d')
  # Table with the average estimated emergence differences from Baseline (from 3 different spawn days)
  EmrgncDifSDAvgTbl <- ggplot(EmDifSDYrAvg %>% filter(Alt != 'Baseline',
                                                      Alt %in% names(altDirs)),
                  aes(x=Year, y=Location, fill=Avg,label=Avg)) +
    geom_tile(alpha=0.8) +
    geom_text(color='black',size=4) +
    facet_grid(.~Alt,scales = 'free') +
    scale_y_discrete(limits=rev) +
    scale_fill_gradient2(low = "orange",mid = "grey",  high = "darkblue",midpoint = 0) +
    ggtitle(paste0('Difference from Baseline in Estimated Emergence Timing (Days)\nAverage of 3 Spawn Days: ',toString(spawndaysMD))) +
    theme(axis.title.x=element_blank(),
          strip.text.y = element_text(angle = 0),
          legend.position ='none',
          axis.text.x=element_text(angle=45,hjust=1))
  EmrgncDifSDAvgTbl
  ggsave(plot=EmrgncDifSDAvgTbl,device = 'png',width=10,height=5,
         filename=file.path(compDir,gsub('Comp','EmrgncDifTbl',pltNm)))
  }
  
  # Summarize by Alt
  for(alt in levels(MoAvg$Alt)){
    print(alt)
    pltNmAlt <- 'TempComp.png'

    # Table with the monthly temp
    MoMeanTbl <- ggplot(MoAvg %>% filter(Alt == alt,Stat == 'MonthlyMean',
                                         !grepl('Jan|Feb|Mar|Dec',Month), 
                                        stringr::str_detect(Location,paste(Sts3ltr,collapse='|'))),
                       aes(x=Month, y=Location, fill=value,label=round(value))) +
      geom_tile(alpha=0.6) +
      geom_text(color='black',size=4) +
      facet_grid(.~Year,scales = 'free') +
      scale_y_discrete(limits=rev) +
      ggtitle(paste0(alt ,' Monthly Mean Water Temperature [deg F]')) +
      theme(axis.title.x=element_blank(),
            strip.text.y = element_text(angle = 0),
            legend.position ='none',
            axis.text.x=element_text(angle=45,hjust=1))
    ggsave(plot=MoMeanTbl,device = 'png',width=10,height=5,
           filename=file.path(compDir,gsub('Comp',paste0('MoMeanTbl',alt),pltNmAlt)))
    if(alt !='Baseline'){
      
      if(alt == 'Measured'){
        MoAvgDifAlt <- MoAvgDif %>% filter(Stat == 'MonthlyMean',
                                           stringr::str_detect(Location,paste(Sts3ltr,collapse='|')))
      }else{
        MoAvgDifAlt <- MoAvgDif %>% filter(Alt == paste0(alt,'_Dif'),
                                           Stat == 'MonthlyMean',
                                           !grepl('Jan|Feb|Mar|Dec',Month), 
                                           stringr::str_detect(Location,paste(Sts3ltr,collapse='|')))
      }
      # Table with the monthly temp diff from Baseline
      MoAvgDifTbl <- ggplot(MoAvgDifAlt,
                            aes(x=Month, y=Location, fill=value,label=round(value))) +
        geom_tile(alpha=0.5) +
        geom_text(color='black',size=4) +
        facet_grid(.~Year,scales = 'free') +
        scale_y_discrete(limits=rev) +
        scale_fill_gradient2(low = "darkblue",mid = "grey", high = "orange",midpoint = 0) +
        ggtitle(paste0('Monthly Mean of Daily Mean \nWater Temperature [deg F] Difference from Baseline (',alt,'-Baseline)')) +
        theme(axis.title.x=element_blank(),
              strip.text.y = element_text(angle = 0),
              legend.position ='none',
              axis.text.x=element_text(angle=45,hjust=1))
      ggsave(plot=MoAvgDifTbl,device = 'png',width=10,height=5,
             filename=file.path(compDir,gsub('Comp',paste0('MoAvgDifTbl',alt),pltNmAlt)))
    }
 
}


  # Summarize by Year
  for(y in levels(ExcDif$Year)){
    }
  #
  #if(any(c('Baseline','Alt1','Alt2a','Alt2b','Alt3a','Alt3b','Alt4','Alt5')%in%names(altDirs))
  #   ){

    # Write csv emergence output for Preferred Alternative Selection
    write.csv( EmDifSDYrAvg %>%
                mutate(Year = as.factor(Year)) %>%
               pivot_wider(id_cols = c('Location','Year'),
                            names_from = 'Alt',
                            values_from = 'Avg') ,
              row.names = F,
              file = file.path(compDir,gsub('.png','.csv',gsub('Comp','EmergenceDaysDifferenceFromBaselineTbl',pltNm))))


    # Write csv output for Preferred Alternative Selection

    TtTimeSum <- TtTime %>%
      bind_rows(TtTime %>% group_by(LifeStage,Location,Alt) %>%
                  dplyr::summarize(DaysNearTarg = mean(DaysNearTarg)) %>%
                  mutate(Year = '3YrAvg'))
    
    # Write CSV for days near target
    write.csv(TtTimeSum %>%
                mutate(Year = as.factor(Year)) %>%
                #filter(Location == 'BCL',grepl('Baseline|Alt1',Alt),!grepl('3YrAvg',Year)) %>%
                filter(stringr::str_detect(Location,"DEX|CGR|FOS|BCL"),
                       !grepl('3YrAvg',Year)) %>%
                group_by(LifeStage,Alt,Location) %>%
                dplyr::summarize(TempResilience = sd(DaysNearTarg)/mean(DaysNearTarg)) %>%
                mutate(TempResilience = round(TempResilience,2)) %>%
                arrange(factor(Location,ordered=T,levels = c('BCL','FOS','CGR','DEX'))) %>%
                arrange(factor(LifeStage,ordered=T,levels = c('DaysNearTargAprAug','DaysNearTargSepMar'))) %>%
                pivot_wider(id_cols = c('Location','LifeStage'),
                            names_from = 'Alt',
                            values_from = 'TempResilience'), #%>%
                #select(Location,Baseline,Alt1,Alt2a,Alt2b,Alt3a,Alt3b,Alt4,Alt5,NTOM),
              row.names = F,
              file = file.path(compDir,gsub('.png','.csv',gsub('Comp',paste0('CVDaysNear',ttType,'TargTbl'),pltNm))))

    library(openxlsx)
    WTxlsxList <- list()
    #browser()

    WTxlsxList$WT_EmergenceDif =  EmDifSDYrAvg %>%
      mutate(Year = as.factor(Year)) %>%
      filter(!grepl('Baseline',Alt)) %>%
      pivot_wider(names_from = 'Alt',
                  values_from = 'Avg')
    
    WTxlsxList$DysBlwSmmrThrshDifTblAvg <- ExcDif %>%
      filter(grepl('DaysBelow18C',LifeStage),
             !grepl('Prc',LifeStage)) %>%
      pivot_wider(names_from = c('Alt'),
                  values_from = 'value')
    
    WTxlsxList$DaysOnTtargSumDifTbl <- TtDifYr %>%
      pivot_wider(names_from = c('Alt'),
                  values_from = 'value')
    
    WTxlsxList$WT_TimeNearTargSum <- TtTime %>%
      bind_rows(TtTime %>% group_by(LifeStage,Location,Alt) %>%
                  dplyr::summarize(DaysNearTarg = mean(DaysNearTarg)) %>%
                  mutate(Year = '3YrAvg')) %>%
      filter(grepl("DEX|CGR|FOS|BCL",Location),
             grepl('3YrAvg',Year)) %>%
      select(-Year) %>%
      mutate(DaysNearTarg = round(DaysNearTarg,1)) %>%
      arrange(factor(Location,ordered=T,levels = c('BCL','FOS','CGR','DEX'))) %>%
      arrange(factor(LifeStage,ordered=T,levels = c('DaysNearTargAprAug','DaysNearTargSepMar'))) %>%
      pivot_wider(id_cols = c('Location','LifeStage','TargNm'),
                  names_from = 'Alt',
                  values_from = 'DaysNearTarg')# %>%
      #select(Location,Baseline,Alt1,Alt2a,Alt2b,Alt3a,Alt3b,Alt4,Alt5,NTOM)

    WTxlsxList$WT_DysBlw18CImpacts <- Timpacts %>%
      filter(grepl('DaysBelow18C',Season)) %>%
      pivot_wider(names_from = c('Alt'),
                  values_from = 'Impact')
    WTxlsxList$WT_DaysNearTargAprAug <- Timpacts %>%
      filter(grepl('DaysNearTargAprAug',Season)) %>%
      pivot_wider(names_from = c('Alt'),
                  values_from = 'Impact')
    WTxlsxList$WT_DaysNearTargSepMar <- Timpacts %>%
      filter(grepl('DaysNearTargSepMar',Season)) %>%
      pivot_wider(names_from = c('Alt'),
                  values_from = 'Impact')
 browser()
    openxlsx::write.xlsx(WTxlsxList,
                         file = file.path(compDir,gsub('.png','.xlsx',gsub('Comp','WTempSummary',pltNm))),
                         asTable = TRUE,overwrite = T,append=T)


  #}

}



 ### Code for temperature impacts if you want to look at all instances (years), not just the 3YrAvg:


# I2 <- function(ex = DasyBelowThresh, em = EggEmergenceDifAvg){
#   emm <- which.max(unlist(abs(em)))
#   exm <- which.max(unlist(abs(ex)))
#   if(any(abs(ex) > TThresh$ExcVal1 |
#          abs(em) > TThresh$EmrgncVal1) ){
#     if(any(abs(ex) > TThresh$ExcVal3 |
#            abs(em) > TThresh$EmrgncVal3) ){
#       which_crit <- c(any(abs(ex) > TThresh$ExcVal3),
#                       any(abs(em) > TThresh$EmrgncVal3))
#       i=Impacts[4]
#     }else{
#       if(any(abs(ex) > TThresh$ExcVal2 |
#              abs(em) > TThresh$EmrgncVal2) ){
#         which_crit <- c(any(abs(ex) > TThresh$ExcVal2),
#                         any(abs(em) > TThresh$EmrgncVal2))
#         i=Impacts[3]
#       }else{
#         which_crit <- c(any(abs(ex) > TThresh$ExcVal1),
#                         any(abs(em) > TThresh$EmrgncVal1))
#         i=Impacts[2]
#       }
#     }
#   }else{
#     i=Impacts[1]
#   }
#   if(i !='Negligible'){
#     if(any(c(ex[exm],em[emm])[which_crit] > 0)){ # A positive change is a delay in emergence
#       i = paste(i,'Ben')
#     }else{
#       i = paste(i,'Adv')
#     }
#   }
#   return(i)
# }

