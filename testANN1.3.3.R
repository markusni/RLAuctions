#####################################################FUNCTIONS
rm(list=ls()) #Clean-Up!
#library(digest)
library(neuralnet)
{
  defenv.func<-function(minval,maxval,n,sd){
    
    valtrue<-round(runif(1,min = minval,max = maxval),digits = 5)
    sdcurr<-abs(round(rnorm(n,mean=0,sd=sd),digits = 1))
    vals<-abs(round(rnorm(n,mean=valtrue,sd=sdcurr),digits=1))
    
    return(list(valtrue,vals,sdcurr))
  }
  
  defnextnaivebid.func<-function(n,highroller,lastbids,globalmaxbid,val,sd,minbidinc,realprofits){
    lastbids<-lastbids[-1]
    highroller[highroller!=0]<-highroller-1
    
    nextminbid<-globalmaxbid+minbidinc
    player2maxbid<-val[1]
    val<-val[2:(n-1)]
    playermaxbid<-val-(qnorm(realprofits^(1/n)))*sd[-1]
    playermaxbid<-c(player2maxbid,playermaxbid)
    nextbids<-round(((playermaxbid-nextminbid)*(runif(n-1,min=0.5,max=0.95)^3)+nextminbid),digits=1)
    
    optout<-which(nextbids<nextminbid)
    nextbids[optout]<-lastbids[optout]
    nextbids[highroller]<-lastbids[highroller]
    return(nextbids)
  }
  
  auctioneer.func<-function(highroller, valtrue){
    
    #gueltige bids bestimmen
    newbids[which(optout==T)]<-lastbids[which(optout==T)]
    newbids[newbids<=lastbids]<-lastbids[newbids<=lastbids]
    
    optout[which(optout==F)]<-newbids[which(optout==F)]==globalmaxbid   #Fuer Fall minbidinc==0
    optout[which(optout==F)]<-newbids[which(optout==F)]<(globalmaxbid+minbidinc)
    
    if(all(optout==T)){                         #Auktion beendet, da keine neuen Gebote eingereicht wurden
      highroller<-which.max(newbids)
      prize<-round((valtrue-newbids[highroller]),digits=2)
      #Wenn keiner Geboten hat
      if(sum(newbids)==0){
        return(list(T,0,prize,0,optout,newbids))
      }
      
      return(list(T,highroller,prize,globalmaxbid,optout,newbids))
    }else{                                      #Auktion geht weiter, maxgebote werden aktualisiert
      globalmaxbid<-max(newbids)
      optout[highroller]<-F                     #Highroller darf in der Folgerunde auf Gebotserhoehung verzichten
      highroller<-which.max(newbids)
      return(list(F,highroller,0,globalmaxbid,optout,newbids))
    }
    
  }
  
  agentbid.func<-function(exp.df,agentval,agentsd){
    
    explore<-0
    names(explore)<-"explore"
    
    lastbids[lastbids==0]<-0.1
    lastbids<-sort(lastbids[-1],decreasing = T)
    
    currstate<-c(n,bidround,agentval,lastbids,rep(0,11-n),globalmaxbid,agentsd,agentval,minbidinc)
    names(currstate)<-c("n","bidround","agentval",paste("BidP",seq(2,11,1)),"globalmaxbid","agentsd","vtestimate","minbidinc")
    
    lastaction<-agentbiddetails[[2]]
    nextminaction<-(globalmaxbid+minbidinc)/agentval
    
    #Wenn der Agent Hoechstbieter ist, erhöht er nicht sein eigenes Gebot, falls er schon raus ist, braucht er auch nichts zu kalkulieren, ebenso wenn nextminaction>1.4  
    if(as.character(roundresult[[2]])==1||roundresult[[5]][1]==T||(ceiling(nextminaction*100)/100)>(0.95+bidround*0.05)){
      return(list(round(lastaction*agentval,digits=1),lastaction,explore,currstate,"Epsilon n/a[1]"))
    }
    
    #amdata<-match(NA, exp.df[,1],nomatch = (nrow(agentexp.df)+1))-1
    
    #Zunächst muss es Ausgangsdaten geben
    if(auc_counter<=NNUpdateCycle){
      ifelse(nextminaction>=(0.95+bidround*0.05),action<-lastaction,action<-round(runif(1,min=nextminaction,max=(0.95+bidround*0.05)),digits=2))
      return(list(round(action*agentval,digits=1),action,explore,currstate,"Epsilon n/a[data]")) #Vector() ist Platzhalter für zusätzliche Werte
    }
    
    #DANN:
    
    #VtEstimate muss berechnet werden
    vtmaxs<-unlist(vt_nn[[length(vt_nn)-1]])
    vtmins<-unlist(vt_nn[[length(vt_nn)]])
    
    vtmaxs_vt<-vtmaxs[length(vtmaxs)]
    vtmins_vt<-vtmins[length(vtmins)]
    
    vtmaxs<-vtmaxs[-length(vtmaxs)]
    vtmins<-vtmins[-length(vtmins)]
    
    scaled <- as.data.frame(scale(t(as.data.frame(currstate[2:15])), center = vtmins, scale = vtmaxs - vtmins))
    vt_estimate<-unname(round(as.vector(compute(vt_nn,scaled)$net.result)*(vtmaxs_vt-vtmins_vt)+vtmins_vt,digits=1))
    
    compstate<-c(currstate[c(1,2,3,14,15)],vt_estimate,currstate[17])
    
    #Maximieren von reward: E(r)=arg max action, f(features,action): E´(r)=delr/delaction, mittels iteration Netzwerk
    
    greedyaction<-maximizeq.func(nn=action_nn, state=compstate, maxs=unlist(action_nn[[length(action_nn)-1]]), 
                                 mins=unlist(action_nn[[length(action_nn)]]), nextminaction, lastaction)
    action<-greedyaction
    
    #Set EPSILON
    #dumamdata<-c(4000,4000,20,20)
    #dumreldev<-c(100,5,100,5)
    
    #QRewards<-1/((dumamdata+50*sqrt(dumreldev))/(50*sqrt(dumreldev)+(250/(dumamdata+250))*dumamdata))
    #QRewards<-(50*sqrt(dumreldev)+(250/(dumamdata+250))*dumamdata)/(dumamdata+50*sqrt(dumreldev))
    #round(matrix(c(0,dumreldev[1:2],dumamdata[1],QRewards[1:2],dumamdata[3],QRewards[3:4]),ncol=3,byrow=T),digits=3)
    
    #epsilon<-round((8*currdev_action+(10*amdata)/(10+amdata)) / (15*(2000*currdev_action)^(1/4)+amdata)+0.01,digits = 3)
    epsilon<-round((1200*currdev_action+(10*amdata)/(10+amdata)) / (15*(300000*currdev_action)^(1/4)+amdata)+0.01,digits = 3)
    
    if(epsilon>0.5)epsilon<-0.5
    
    if(runif(1,min=0,max=1000)<=epsilon*1000){
      explore<-1
      print("EXPLORING ACTION")
      #Ist greedy = last action und es soll exploriert werden, werden als Nachbarstates die ersten wieder gültigen States (ab nextminaction) herangenommen
      exploreopts<-c(lastaction,seq(ceiling(nextminaction*100)/100,0.95+bidround*0.05,0.01))
      if(!is.na(match(as.character(greedyaction),exploreopts)))exploreopts<-exploreopts[-match(as.character(greedyaction),exploreopts)]
      if(length(exploreopts>1))action<-sample(exploreopts,1)
    }
    
    return(list(round(action*agentval,digits=1),action,explore,currstate,epsilon))
    
  }
  
  maximizeq.func<-function(nn, state, maxs, mins, nextminaction, lastaction){
    
    minaction<-mins[names(mins)=="ActionRel"]
    maxaction<-maxs[names(maxs)=="ActionRel"]
    
    mins<-mins[1:(length(mins)-2)]
    maxs<-maxs[1:(length(maxs)-2)]
    #(1-0.91)/(1.11-0.91)
    
    state.scaled<-as.data.frame(scale(t(as.data.frame(state)),center = mins, scale = maxs - mins))
    state.scaled<-cbind(state.scaled,0)
    
    nextminaction_c<-ceiling(nextminaction*10)/10
    
    possactions<-c(lastaction,seq(nextminaction_c,1.5,0.1))
    
    expres<-vector()
    
    for(i in 1:length(possactions)){
      
      scaledaction<-as.vector(scale(possactions[i],center=minaction,scale=maxaction-minaction))
      
      state.scaled[length(state.scaled)]<-scaledaction
      
      expres[i]<-as.vector(compute(nn,state.scaled)$net.result)
      
    }
    
    maxexpr_action<-possactions[which.max(expres)]
    if(as.character(maxexpr_action)==lastaction)maxexpr_action<-nextminaction_c
    
    possactions<-seq(maxexpr_action-0.09,maxexpr_action+0.09,0.01)
    possactions<-c(lastaction,possactions[possactions>nextminaction])
    
    expres<-vector()
    
    for(i in 1:length(possactions)){
      
      scaledaction<-as.vector(scale(possactions[i],center=minaction,scale=maxaction-minaction))
      
      state.scaled[length(state.scaled)]<-scaledaction
      
      expres[i]<-as.vector(compute(nn,state.scaled)$net.result)
      
    }
    
    return(possactions[which.max(expres)])
    
  }
  
  vtlearn.func<-function(exp.df,agentval){
    
    tracer_l<-length(tracer)
    
    if(tracer_l==0){
      return(exp.df)
    }
    
    lastaction<-agentbiddetails[[2]]
    
    if(as.character(roundresult[[2]])==1){
      vtrue<-round(lastaction*agentval+roundresult[[3]],digits=1)
    }else{
      return(exp.df)
    }
    
    #c("Round","AgentVal",paste("Maxbid", 2:11,sep=""),"Globalmaxbid","Vtrue")
    new_states<-vector()
    amdata<-match(NA, exp.df[,1],nomatch = (nrow(exp.df)+1))-1
    
    for(i in 1:tracer_l){
      
      update<-c(tracer[[i]][c(1:15)],vtrue)
      
      #Suche nach Vorerfahrungen für die Updates
      relevantstates<-which(exp.df$Nplayers==as.character(update[1]))
      startcol<-which(colnames(exp.df)=="Round")
      similarities<-similar.func(tempmatrix = exp.df[relevantstates,c(startcol:(startcol+n),startcol+12,startcol+13)],currstate=update[c(2:(n+2),14,15)])
      
      relevantstates<-relevantstates[similarities[[1]]]
      
      #Soeben neu geschriebene States werden nicht geupdated
      recentlyadded<-which(!is.na(match(relevantstates,new_states)))
      relevantstates<-relevantstates[-recentlyadded]
      
      #Alte States werden angepasst
      if(as.character(length(relevantstates))>0){
        
        similarities[[2]]<-similarities[[2]][-recentlyadded]
        
        #Anpassung der nun uebrigen States
        exp.df$Vtrue[relevantstates]<-exp.df$Vtrue[relevantstates]+round((1/((similarities[[2]]+1)^(13-alpha*13)))*(vtrue-exp.df$Vtrue[relevantstates]),digits=1)
        exp.df$NUpdates[relevantstates]<-exp.df$NUpdates[relevantstates]+1
        
        #Neue States angelegt mit willkuerlichem Grenzwert fuer Aehnlichkeit 0.02
        if(any(similarities[[2]]<=0.02)==F){
          exp.df[amdata+1,]<-c(update,1)
          new_states<-c(new_states,amdata+1)
        }
        
      }else{
        #Falls kein State bekannt ist wird er neu angelegt
        exp.df[amdata+1,]<-c(update,1)
        new_states<-c(new_states,amdata+1)
      }
      
      amdata<-match(NA, exp.df[,1],nomatch = (nrow(exp.df)+1))-1
      
    }
    
    return(exp.df)
    
  }
  
  vt_updates.func<-function(exp.df){
    
    amdata<-match(NA, exp.df[,1],nomatch = (nrow(exp.df)+1))-1
    
    tempexp.df<-exp.df[1:amdata,c(2:15)]
    
    maxs=unlist(vt_nn[[length(vt_nn)-1]])
    mins=unlist(vt_nn[[length(vt_nn)]])
    
    minvtrue<-mins[names(mins)=="Vtrue"]
    maxvtrue<-maxs[names(mins)=="Vtrue"]
    
    mins<-mins[-15]
    maxs<-maxs[-15]
    
    #Data Skalieren
    tempexp.df <- as.data.frame(scale(tempexp.df, center = mins, scale = maxs - mins))
    
    vtestimators<-round(compute(vt_nn,tempexp.df)$net.result*(maxvtrue-minvtrue)+minvtrue,digits=1)
    
    exp.df$VtEstimator[1:amdata]<-vtestimators
    
    return(exp.df)  
  }
  
  actionlearn.func<-function(exp.df){
    #exp.df<-actionexp.df
    
    tracer_l<-length(tracer)
    explorepath<-F
    
    if(tracer_l==0){
      return(exp.df)
    }
    
    if(as.character(roundresult[[2]])==1){
      newtarget<-roundresult[[3]]
    }else{
      newtarget<-0
    }
    
    #Bei negativen Gewinnen kann nur der Q-Wert der letzten Aktion angepasst werden
    if(newtarget<0){
      tracer<-list(tracer[[tracer_l]])
      tracer_l<-1
    }
    
    new_states<-vector()
    
    for(i in 1:tracer_l){
      
      amdata<-match(NA, exp.df[,1],nomatch = (nrow(exp.df)+1))-1
      explore<-tracer[[tracer_l-i+1]][19]
      update<-tracer[[tracer_l-i+1]][c(1:18)]
      
      #newtarget<-resagent/(1.5-(alpha/2))^(i-1)
      #Ein temp exp.df wird erstellt, wo nur die Vorhersagerelevanten Faktoren drin sind
      rel_exp.df<-exp.df[,-c(4:14,19,20)]
      
      #Suche nach Vorerfahrungen für die Updates
      similarities<-similar.func(tempmatrix = rel_exp.df[1:amdata,],currstate=update[-c(4:14)])
      relevantstates<-unlist(similarities[[1]])
      
      #Soeben neu geschriebene States werden nicht geupdated
      recentlyadded<-which(!is.na(match(relevantstates,new_states)))
      relevantstates<-relevantstates[-recentlyadded]
      
      #Alte States werden angepasst
      if(as.character(length(relevantstates))>=1){
        
        similarities[[2]]<-similarities[[2]][-recentlyadded]
        
        #Falls im Folgenden exploriert wurde, duerfen die Actionvalues nur nach oben angepasst werden
        if(explorepath==T){
          similarities[[2]]<-similarities[[2]][which(exp.df$QReward[relevantstates]<newtarget)]
          relevantstates<-relevantstates[which(exp.df$QReward[relevantstates]<newtarget)]
        }
        
        #Anpassung der nun uebrigen States
        exp.df$QReward[relevantstates]<-exp.df$QReward[relevantstates]+round((1/((similarities[[2]]+1)^(13-alpha*13)))*(newtarget-exp.df$QReward[relevantstates]),digits=3)
        exp.df$NUpdates[relevantstates]<-exp.df$NUpdates[relevantstates]+1
        
        #Neue States angelegt mit willkuerlichem Grenzwert fuer Aehnlichkeit 0.02
        if(any(similarities[[2]]<=0.05)==F){
          update[19]<-round(alpha*newtarget,digits=3)
          exp.df[amdata+1,]<-c(update,1)
          new_states<-c(new_states,amdata+1)
        }
        
      }else{
        #Falls kein State bekannt ist wird er neu angelegt
        update[19]<-round(alpha*newtarget,digits=3)
        exp.df[amdata+1,]<-c(update,1)
        new_states<-c(new_states,amdata+1)
      }
      
      if(as.character(explore)==1)explorepath<-T
      
    }
    return(exp.df)
    
  }
  
  similar.func<-function(tempmatrix, currstate, maxs, mins){ #Beim hier gewaehlten Verfahren (aehnelt Standardabweichung) werden grosse Abweichungen hoeher gewichtet als kleine
    
    if(nrow(tempmatrix)==0){
      return(list(vector()))
    }
    
    sims<-as.matrix(tempmatrix)%*%diag(1/currstate)   #Werte der Matrix werden durch currstate Werte dividiert
    sims<-unname(sqrt(rowSums((sims-1)^2)/ncol(tempmatrix)))
    
    hits<-which(sims<=alpha)
    
    return(list(hits,sims[hits]))
  }
  
  create_nn.func<-function(exp.df,type){
    
    #Lösungsmechanismus: Neurales Netz
    
    amdata<-match(NA, exp.df[,1],nomatch = (nrow(exp.df)+1))-1
    exp.df<-exp.df[1:amdata,]
    
    index<-sample(1:amdata,round(0.8*amdata))
    
    maxs<-apply(exp.df, 2, max) 
    mins<-apply(exp.df, 2, min)
    
    if(type=="action_nn")mins[names(mins)=="ActionRel"]<-0
    
    scaledexp.df<-as.data.frame(scale(exp.df, center = mins, scale = maxs - mins))
    
    training.df<-scaledexp.df[index,]
    testing.df<-scaledexp.df[-index,]
    
    cols<-names(exp.df)
    targetcol<-cols[length(cols)]
    
    formula<-as.formula(paste(targetcol," ~", paste(cols[!cols %in% targetcol], collapse = " + ")))
    
    if(exists(type)){
      old_weights<-unlist(get(type)$weights[[which.min(get(type)$result.matrix[1,])]])
      old_weights<-rep(old_weights,10)
      newnn<-neuralnet(formula,data=training.df,hidden=c(5,3),linear.output=T,rep=10,startweights = old_weights)
    }else{
      newnn<-neuralnet(formula,data=training.df,hidden=c(5,3),linear.output=T,rep=1)
    }
    
    #Voerhersagung um sd zu schätzen
    predictions<-compute(newnn,testing.df[,1:(ncol(testing.df)-1)])$net.result
    realv<-testing.df[,length(cols)]
    pmaxes<-pmax(predictions,realv)
    pmaxes[pmaxes==0]<-0.01
    meanreldev<-sqrt(sum((abs(predictions-realv)/pmaxes)^2)/length(realv))
    
    # predictions<-predictions*(max(exp.df[,targetcol])-min(exp.df[,targetcol]))+min(exp.df[,targetcol])
    # realv<-exp.df[-index,length(cols)]
    # realv[realv==0]<-0.01
    # meanreldev<-sqrt(sum(((as.vector(predictions)/realv)-1)^2)/length(realv))

    newnn[[length(newnn)+1]]<-meanreldev
    newnn[[length(newnn)+1]]<-maxs
    newnn[[length(newnn)+1]]<-mins
    
    return(newnn)
    
  }
}

#####################################################USER SET // UNVERAENDERLICH
{
  minval<-1
  maxval<-100
  alpha<-0.4 #Lernrate [0,1], je hoeher desto, schneller lernt der Agent, vermischt allerdings auch staerker Erfahrungen aus verschiedenen States miteinander
  resprice<-0
  NNUpdateCycle<-300
  realprofits<-0.5
}
###########################################################
{
  actionexp.df<-setNames(data.frame(matrix(ncol = 20, nrow = 10000)), c("NPlayers","Round","AgentVal",paste("Maxbid", 2:11,sep=""),"Globalmaxbid","AgentSD","VtEstimator","Minbidinc","ActionRel","QReward","NUpdates"))
  vt_estimator.df<-setNames(data.frame(matrix(ncol = 17, nrow = 10000)), c("NPlayers","Round","AgentVal",paste("Maxbid", 2:11,sep=""),"Globalmaxbid","AgentSD","Vtrue","NUpdates"))
  balance.df<-setNames(data.frame(1:11, 0, 0),c("Player","Cash","Times Won"))
  timeystats.mtr<-setNames(data.frame(matrix(0,ncol = 12, nrow = 1)), c("Auction",seq(1,11,1)))
  auc_counter<-0
  currdev_vt<-NA
  currdev_action<-NA
  curreps<-NA
}
############################################################COMPUTATION
while(auc_counter%%100!=0 || balance.df[1,2]<=max(balance.df[-1,2])){
  
  #Neue Auktion  
  {
    auc_counter<-auc_counter+1
    globalsd<-round(runif(1,min=5,max=10),digits=1)
    n<-round(runif(1,min=2.5,max=11.49),digits=0) #Spieleranzahl, ergibt 1 Agenten und n-1 Mitspieler
    currenv<-defenv.func(minval,maxval,n,globalsd)
    minbidinc<-round(runif(1,min=0,max=1)^3*currenv[[1]]/5,digits=1)
    globalmaxbid<-resprice+0.1
    lastbids<-rep(0,n) #die letzten Gebote werden wieder auf null gesetzt. lastbids koennte auch currmaxbids heissen
    playersnextbids<-rep(0,n-1)
    optout<-rep(F,n)
    bidround<-0
    roundresult<-list(F,0,0,0,F)
    tracer<-list()
    agentbiddetails<-list(0,0)
  }
  
  while(roundresult[[1]]==F) {
    
    bidround<-bidround+1
    
    test<-agentbid.func(exp.df=actionexp.df,agentval=currenv[[2]][1],agentsd=currenv[[3]][1])
    ifelse(is.na(test[[1]]),break,agentbiddetails<-test)
    
    
    playersnextbids<-defnextnaivebid.func(n,highroller = roundresult[[2]],lastbids,globalmaxbid,val=currenv[[2]][-1],
                                          sd=currenv[[3]][-1],minbidinc, realprofits)
    
    newbids<-c(agentbiddetails[[1]],playersnextbids)
    
    #Auktionator beurteilt
    roundresult<-auctioneer.func(highroller = roundresult[[2]],valtrue = currenv[[1]])
    
    
    #tracer wird ueberschrieben solange der Agent noch drin ist
    if(roundresult[[5]][1]==F){
      tracer[[bidround]]<-c(agentbiddetails[[4]],agentbiddetails[[2]],agentbiddetails[[3]])
    }
    
    globalmaxbid<-roundresult[[4]]
    optout<-roundresult[[5]]
    lastbids<-roundresult[[6]]
    
    if(typeof(agentbiddetails[[5]])=="double")curreps<-unlist(agentbiddetails[[5]])
    
  }
  if(is.na(test[[1]]))break
  if(roundresult[[2]]!=0){
    balance.df[roundresult[[2]],3]<-balance.df[roundresult[[2]],3]+1
    balance.df[roundresult[[2]],2]<-balance.df[roundresult[[2]],2]+roundresult[[3]]
  }
  
  vt_estimator.df<-vtlearn.func(exp.df=vt_estimator.df,agentval = currenv[[2]][1])
  
  actionexp.df<-actionlearn.func(exp.df=actionexp.df)
  
  amdata<-match(NA, actionexp.df[,1],nomatch = (nrow(actionexp.df)+1))-1
  
  ################Ende ANPASSUNGEN 
  
  
  if(roundresult[[2]]==1){
    cat("\n","\n","Der Agent hat in Auktion ",auc_counter,", Runde ",bidround," soeben ",roundresult[[3]]," verwirklicht.",
        "\n","Gebot: ",globalmaxbid, " bei Vtrue ",currenv[[1]],".","\n",sep="")
    
  }
  
  if(auc_counter%%100==0){
    
    amdata_vt<-match(NA, vt_estimator.df[,1],nomatch = (nrow(vt_estimator.df)+1))-1
    
    cat("\n","\n","Auktion No.",auc_counter,
        "\n","Gelernte states:",amdata,"| Daten für vt:",amdata_vt,
        "\n","Mean Relative Deviation(vt_nn):",currdev_vt,"| Mean Relative Deviation(action_nn):",currdev_action,
        "\n","Letztes Epsilon:",curreps,"\n","Balance:","\n",sep = " ")
    print(t(balance.df[,-1]))
    
    cat("\n","Aktuelle Korrelationen mit Vt:","\n")
    print(cor(vt_estimator.df[1:amdata_vt,-c(16,17)],vt_estimator.df$Vtrue[1:amdata_vt]))
    
    timeystats.mtr<-rbind(timeystats.mtr,c(auc_counter,t(balance.df)[2,]))
    
    plot(timeystats.mtr[,1:2],ylab="Balance (red=Agent)",xlab="Auction",type="l",col="red",
         ylim=c(min(timeystats.mtr[,2:12])-20,max(timeystats.mtr[,2:12])+20))
    for(i in 3:12){
      lines(timeystats.mtr[,c(1,i)],col=(ifelse(i==3,"green3","black")))
    }
  }
  
  if(auc_counter%%NNUpdateCycle==0){
    
    cat("\n","UPDATING VT NN...","\n")
    
    vt_nn<-create_nn.func(exp.df=vt_estimator.df[,2:16],type="vt_nn")
    
    currdev_vt<-vt_nn[[length(vt_nn)-2]]
    
    cat("\n","DONE! UPDATING V_ESTIMATORS IN ACTION DF","\n")
    
    actionexp.df<-vt_updates.func(exp.df = actionexp.df)
    
    cat("\n","DONE! UPDATING ACTION NN","\n")
    
    #QRewards in actionexp.df werden mit dem Faktor Wurzel(x) skaliert (Genauigkeit bei kleineren Werten ist relevanter!)
    scaledQ<-round(sqrt(abs(actionexp.df$QReward[1:amdata])),digits=2)
    scaledQ[actionexp.df$QReward[1:amdata]<0]<-scaledQ[actionexp.df$QReward[1:amdata]<0]*(-1)
    
    action_nn<-create_nn.func(exp.df=cbind(actionexp.df[1:amdata,c(1:3,14:18)],scaledQ),type="action_nn")
    
    currdev_action<-action_nn[[length(vt_nn)-2]]
    
    cat("GOING ON...","\n")
  }
}

plot(currnn,rep = "best")