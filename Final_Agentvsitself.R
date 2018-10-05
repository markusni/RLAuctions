#rm(list=ls()) #Clean-Up!
library(neuralnet)

#####################################################FUNCTIONS####
{
  defenv.func<-function(minval,maxval,n,sd){
    
    valtrue<-round(runif(1,min = minval,max = maxval),digits = 5)
    sdcurr<-abs(round(rnorm(n,mean=0,sd=sd),digits = 1))
    vals<-abs(round(rnorm(n,mean=valtrue,sd=sdcurr),digits=1))
    
    return(list(valtrue,vals,sdcurr))
  }
  
  calc_epsilon.func<-function(timeystats.mtr){
    
    if(auc_counter<NNUpdateCycle)return(NA)
    
    epsilon<-vector()
    for(i in 1:11){
      balance_diffs<-c(0,diff(timeystats.mtr[,i+1]))
      maxgain<-max(0,tail(sort(balance_diffs[-1]),5))
      mingain<-mean(balance_diffs[2:(NNUpdateCycle/100+1)])
      balance_diffs_l<-length(balance_diffs)
      last_mov_average<-ifelse(balance_diffs_l<6,mean(balance_diffs[2:balance_diffs_l]),mean(balance_diffs[(balance_diffs_l-4):balance_diffs_l]))
    
      if(last_mov_average<mingain)last_mov_average<-mingain
      scaled<-(last_mov_average-mingain)/(maxgain-mingain)
      
      epsilon<-c(epsilon,round(0.51-(0.5-(200/(auc_counter+400))),digits=3))
    }
    if(is.nan(epsilon))epsilon[which(is.nan(epsilon))]<-0.51
    return(epsilon)
  }
  
  auctioneer.func<-function(highroller, valtrue){
    
    #gueltige bids bestimmen
    newbids[which(optout==T)]<-lastbids[which(optout==T)]
    newbids[newbids<=lastbids]<-lastbids[newbids<=lastbids]
    
    optout[which(optout==F)]<-newbids[which(optout==F)]<(globalmaxbid*1.05)
    
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
  
  agentbid.func<-function(exp.df,agentval,agentsd,player){
    
    explore<-0
    names(explore)<-"explore"
    epsilon<-round(0.51-(0.5-(200/(auc_counter+400))),digits=3)
    
    lastbids[lastbids==0]<-0.1
    lastbids<-sort(lastbids[-player],decreasing = T)
    
    currstate<-c(n,bidround,agentval,lastbids,rep(0,11-n),globalmaxbid,agentsd,agentval)
    names(currstate)<-c("n","bidround","agentval",paste("BidP",seq(2,11,1)),"globalmaxbid","agentsd","vtestimate")
    
    lastaction<-get(paste("agentbiddetailsp",player,sep=""))[[2]]
    
    nextminaction<-ceiling((globalmaxbid*1.05*100)/agentval)/100
    
    #Wenn der Agent Hoechstbieter ist, erhoeht er nicht sein eigenes Gebot, falls er schon raus ist, braucht er auch nichts zu kalkulieren, ebenso wenn nextminaction>1.4  
    if(as.character(roundresult[[2]])==player||roundresult[[5]][player]==T||nextminaction>1.2){
      return(list(round(lastaction*agentval,digits=1),lastaction,explore,currstate,"Epsilon n/a[1]"))
    }
    
    #amdata<-match(NA, exp.df[,1],nomatch = (nrow(agentexp.df)+1))-1
    
    #Zunaechst muss es Ausgangsdaten geben
    if(auc_counter<=NNUpdateCycle){
      ifelse(nextminaction>1.2,action<-lastaction,action<-round(nextminaction+(1.2-nextminaction)*runif(1,min=0.01,max=0.99)^5,digits=2))
      return(list(round(action*agentval,digits=1),action,explore,currstate,"Epsilon n/a[data]"))
    }
    
    #DANN:
    
    #VtEstimate muss berechnet werden
    vtmaxs<-unlist(vt_nn[[length(vt_nn)-1]])
    vtmins<-unlist(vt_nn[[length(vt_nn)]])
    
    vtmaxs_vt<-vtmaxs[length(vtmaxs)]
    vtmins_vt<-vtmins[length(vtmins)]
    
    vtmaxs<-vtmaxs[-length(vtmaxs)]
    vtmins<-vtmins[-length(vtmins)]
    
    scaled <- as.data.frame(scale(t(as.data.frame(currstate[1:15])), center = vtmins, scale = vtmaxs - vtmins))
    vt_estimate<-unname(round(as.vector(compute(vt_nn,scaled)$net.result)*(vtmaxs_vt-vtmins_vt)+vtmins_vt,digits=1))
    
    currstate[16]<-vt_estimate
    
    compstate<-currstate[c(1:3,14:16)]
    
    #Maximieren von reward: E(r)=arg max action, f(features,action): E?(r)=delr/delaction, mittels iteration Netzwerk
    
    greedyaction<-maximizeq.func(nn=action_nn, state=compstate, maxs=unlist(action_nn[[length(action_nn)-1]]), 
                                 mins=unlist(action_nn[[length(action_nn)]]), nextminaction, lastaction, agentval)
    action<-greedyaction
    
    #EXPLORE
    
    if(runif(1,min=0,max=1000)<=epsilon*1000&&nextminaction<=1.05){
      explore<-1
      print("EXPLORING ACTION")
      
      #Ist greedy = last action und es soll exploriert werden, werden als Nachbarstates die ersten wieder gueltigen States (ab nextminaction) herangenommen
      exploreopts<-c(lastaction,seq(nextminaction,1.05,0.01))
      if(!is.na(match(as.character(greedyaction),exploreopts)))exploreopts<-exploreopts[-match(as.character(greedyaction),exploreopts)]
      if(length(exploreopts>1))action<-sample(exploreopts,1)
    }
    
    return(list(round(action*agentval,digits=1),action,explore,currstate,epsilon))
    
  }
  
  maximizeq.func<-function(nn, state, maxs, mins, nextminaction, lastaction, agentval){
    
    minactionRel<-mins[names(mins)=="ActionRel"]
    maxactionRel<-maxs[names(maxs)=="ActionRel"]
    
    minreward<-mins[names(mins)=="QReward"]
    maxreward<-maxs[names(maxs)=="QReward"]
    
    mins<-mins[1:(length(mins)-2)]
    maxs<-maxs[1:(length(maxs)-2)]
    #(1-0.91)/(1.11-0.91)
    
    state.scaled<-as.data.frame(scale(t(as.data.frame(state)),center = mins, scale = maxs - mins))
    state.scaled<-cbind(state.scaled,0)
    
    nextminaction_c<-round(nextminaction,digits=1)
    
    possactions<-c(lastaction,seq(nextminaction_c,1.2,0.1))
    
    expres<-vector()
    
    for(i in 1:length(possactions)){
      
      scaledactionRel<-as.vector(scale(possactions[i],center=minactionRel,scale=maxactionRel-minactionRel))
      
      state.scaled[length(state.scaled)]<-scaledactionRel
      
      expres[i]<-as.vector(compute(nn,state.scaled)$net.result)
      
    }
    
    expres<-expres*(maxreward-minreward)+minreward
    
    #lastaction bringt immer 0!
    expres[1]<-0
    
    maxexpr_action<-possactions[which.max(expres)]
    if(as.character(maxexpr_action)==lastaction)maxexpr_action<-nextminaction_c
    
    possactions<-seq(maxexpr_action-0.09,maxexpr_action+0.09,0.01)
    possactions<-possactions[possactions<=1.2]
    possactions<-c(lastaction,possactions[possactions>=nextminaction])
    
    expres<-vector()
    
    for(i in 1:length(possactions)){
      
      scaledactionRel<-as.vector(scale(possactions[i],center=minactionRel,scale=maxactionRel-minactionRel))
      
      state.scaled[length(state.scaled)]<-scaledactionRel
      
      expres[i]<-as.vector(compute(nn,state.scaled)$net.result)
      
    }
    
    expres<-expres*(maxreward-minreward)+minreward
    
    #lastaction bringt immer 0!
    expres[1]<-0
    
    return(possactions[which.max(expres)])
    
  }
  
  vtlearn.func<-function(exp.df,agentval,player){
    
    tracer<-get(paste("tracerp",player,sep=""))
    
    tracer_l<-length(tracer)
    
    if(tracer_l==0){
      return(exp.df)
    }
    
    lastaction<-get(paste("agentbiddetailsp",player,sep=""))[[2]]
    
    if(as.character(roundresult[[2]])==k){
      vtrue<-round(lastaction*agentval+roundresult[[3]],digits=1)
    }else{
      if(as.character(tracer[[1]][15])==0){
        vtrue<-agentval
      }else{
        return(exp.df)
      }
    }
    
    
    #Ein temp exp.df wird erstellt, wo nur die Vergleichsrelevanten Faktoren drin sind
    amdata<-match(NA, exp.df[,1],nomatch = (nrow(exp.df)+1))-1
    startcol<-which(colnames(exp.df)=="Round")
    rel_exp.df<-exp.df[1:amdata,c(startcol:(startcol+n),startcol+12,startcol+13)]
    
    new_states<-vector()
    for(i in 1:tracer_l){
      
      update<-c(tracer[[i]][c(1:15)],vtrue)
      
      #Suche nach Vorerfahrungen f?r die Updates
      relevantstates<-which(exp.df$NPlayers==as.character(update[1]))
      
      #Soeben neu geschriebene States werden nicht geupdated
      recentlyadded<-which(!is.na(match(relevantstates,new_states)))
      if(length(recentlyadded)>0)relevantstates<-relevantstates[-recentlyadded]
      
      startcol<-which(colnames(exp.df)=="Round")
      similarities<-similar.func(tempmatrix = rel_exp.df[relevantstates,],currstate=update[c(2:(n+2),14,15)],
                                 maxs=apply(rel_exp.df, 2, max),mins=apply(rel_exp.df, 2, min))
      
      relevantstates<-relevantstates[similarities[[1]]]
      
      #Alte States werden angepasst
      if(length(relevantstates)>0){
        
        if(length(recentlyadded)>0)similarities[[2]]<-similarities[[2]][-recentlyadded]
        
        #Anpassung der nun uebrigen States
        exp.df$Vtrue[relevantstates]<-round(exp.df$Vtrue[relevantstates]+(1/((similarities[[2]]+1)^(20-alpha*20)))*alpha*(vtrue-exp.df$Vtrue[relevantstates]),digits=3)
        exp.df$NUpdates[relevantstates]<-exp.df$NUpdates[relevantstates]+1
        
        #Neue States angelegt mit willkuerlichem Grenzwert fuer Aehnlichkeit 0.05%
        if(any(similarities[[2]]<=0.05)==F){
          exp.df[amdata+1,]<-c(update,1)
          new_states<-c(new_states,amdata+1)
        }
        
      }else{
        #Falls kein State bekannt ist wird er auch neu angelegt
        exp.df[amdata+1,]<-c(update,1)
        new_states<-c(new_states,amdata+1)
      }
      
      amdata<-match(NA, exp.df[,1],nomatch = (nrow(exp.df)+1))-1
      
    }
    
    return(exp.df)
    
  }
  
  vt_updates.func<-function(exp.df){
    
    amdata<-match(NA, exp.df[,1],nomatch = (nrow(exp.df)+1))-1
    
    tempexp.df<-exp.df[1:amdata,c(1:15)]
    
    maxs=unlist(vt_nn[[length(vt_nn)-1]])
    mins=unlist(vt_nn[[length(vt_nn)]])
    
    minvtrue<-mins[names(mins)=="Vtrue"]
    maxvtrue<-maxs[names(mins)=="Vtrue"]
    
    mins<-mins[-16]
    maxs<-maxs[-16]
    
    #Data Skalieren
    tempexp.df <- as.data.frame(scale(tempexp.df, center = mins, scale = maxs - mins))
    
    vtestimators<-round(compute(vt_nn,tempexp.df)$net.result*(maxvtrue-minvtrue)+minvtrue,digits=1)
    
    exp.df$VtEstimator[1:amdata]<-vtestimators
    
    return(exp.df)  
  }
  
  actionlearn.func<-function(exp.df,player){
    #exp.df<-actionexp.df
    
    tracer<-get(paste("tracerp",player,sep=""))
    
    tracer_l<-length(tracer)
    explorepath<-F
    
    if(tracer_l==0){
      return(exp.df)
    }
    
    if(as.character(roundresult[[2]])==player){
      newtarget<-roundresult[[3]]
    }else{
      newtarget<-0
    }
    
    #Bei negativen Gewinnen kann nur der Q-Wert der letzten Aktion angepasst werden
    if(newtarget<0){
      tracer<-list(tracer[[tracer_l]])
      tracer_l<-1
    }
    
    #Ein temp exp.df wird erstellt, wo nur die Vergleichsrelevanten Faktoren drin sind
    amdata<-match(NA, exp.df[,1],nomatch = (nrow(exp.df)+1))-1
    startcol<-which(colnames(exp.df)=="NPlayers")
    rel_exp.df<-exp.df[1:amdata,c(startcol:(startcol+2),(startcol+13):(startcol+16))]
    
    new_states<-vector()
    for(i in 1:tracer_l){
      
      currtarget<-1/(1+(0.5-alpha/2))^(i-1)*newtarget
      
      amdata<-match(NA, exp.df[,1],nomatch = (nrow(exp.df)+1))-1
      
      explore<-tracer[[tracer_l-i+1]][18]
      update<-tracer[[tracer_l-i+1]][c(1:17)]
      
      #Suche nach Vorerfahrungen fuer die Updates
      similarities<-similar.func(tempmatrix = rel_exp.df[1:amdata,],currstate=update[-c(4:13)],
                                 maxs=apply(rel_exp.df, 2, max),mins=apply(rel_exp.df, 2, min))
      relevantstates<-unlist(similarities[[1]])
      
      #Soeben neu geschriebene States werden nicht geupdated
      recentlyadded<-which(!is.na(match(relevantstates,new_states)))
      if(length(recentlyadded)>0)relevantstates<-relevantstates[-recentlyadded]
      
      #Alte States werden angepasst
      if(length(relevantstates)>=1){
        
        if(length(recentlyadded)>0)similarities[[2]]<-similarities[[2]][-recentlyadded]
        
        #Falls im Folgenden exploriert wurde, duerfen die Actionvalues nur nach oben angepasst werden
        if(explorepath==T){
          similarities[[2]]<-similarities[[2]][which(exp.df$QReward[relevantstates]<currtarget)]
          relevantstates<-relevantstates[which(exp.df$QReward[relevantstates]<currtarget)]
        }
      }
      
      if(length(relevantstates)>1){
        #Anpassung der nun uebrigen States
        exp.df$QReward[relevantstates]<-round(exp.df$QReward[relevantstates]+(1/((similarities[[2]]+1)^(20-alpha*20)))*alpha*(currtarget-exp.df$QReward[relevantstates]),digits=4)
        exp.df$NUpdates[relevantstates]<-exp.df$NUpdates[relevantstates]+1
        
        #Neue States angelegt mit willkuerlichem Grenzwert fuer Aehnlichkeit 0.05%
        if(any(similarities[[2]]<=0.05)==F){
          update[18]<-round(alpha*currtarget,digits=4)
          exp.df[amdata+1,]<-c(update,1)
          new_states<-c(new_states,amdata+1)
        }
        
      }else{
        #Falls kein State bekannt ist wird er neu angelegt
        update[18]<-round(alpha*currtarget,digits=4)
        exp.df[amdata+1,]<-c(update,1)
        new_states<-c(new_states,amdata+1)
      }
      
      if(as.character(explore)==1)explorepath<-T
      
    }
    return(exp.df)
    
  }
  
  similar.func<-function(tempmatrix, currstate, maxs, mins){
    
    amdata<-match(NA, tempmatrix[,1],nomatch = (nrow(tempmatrix)+1))-1
    
    if(amdata<1){
      return(list(vector()))
    }
    
    #tempmatrix und currstate werden skaliert
    scaledexp.df<-as.data.frame(scale(tempmatrix, center = mins, scale = maxs - mins))
    if(any(is.na(scaledexp.df)))return(list(vector())) #Falls Daten zum skalieren nicht ausreichen
    state.scaled<-as.vector(scale(t(as.data.frame(currstate)), center = mins, scale = maxs - mins))
    
    distances<-abs(sweep(scaledexp.df,2,state.scaled))
    
    #Euklidisch!
    euklidean<-sqrt(as.vector(rowSums(distances^2)))
    hits<-which(euklidean<0.1)
    sims<-euklidean[hits]
    
    #hits<-unname(which(apply(distances,1,function(x)all(x<=0.05))))
    #sims<-unname(sqrt(rowSums(distances[hits,]^2)/ncol(distances))*10)
    
    return(list(hits,sims))
  }
  
  create_nn.func<-function(exp.df,type){
    
    #Loesungsmechanismus: Neurales Netz
    
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
      set_weights<-unlist(get(type)$weights[[which.min(get(type)$result.matrix[1,])]])
      ifelse(type=="action_nn",addreps<-1,addreps<-0)
      set_weights<-c(set_weights,runif(addreps*length(set_weights),min=1,max=3))
      newnn<-neuralnet(formula,data=training.df,hidden=c(5,3),stepmax = 50e+04,linear.output=T,rep=(addreps+1),startweights = set_weights, lifesign = "full")
      which.min(newnn$result.matrix[1,])
    }else{
      newnn<-neuralnet(formula,data=training.df,hidden=c(5,3),stepmax = 25e+04,linear.output=T,rep=5,lifesign = "full")
    }
    
    #Voerhersagung um sd zu schaetzen
    predictions<-as.vector(compute(newnn,testing.df[,1:(ncol(testing.df)-1)])$net.result)
    
    realv<-testing.df[,length(cols)]
    
    meanreldev<-sqrt(sum((abs(predictions-realv))^2)/length(realv))
    
    
    newnn[[length(newnn)+1]]<-meanreldev
    newnn[[length(newnn)+1]]<-maxs
    newnn[[length(newnn)+1]]<-mins
    
    return(newnn)
    
  }
  
}

#####################################################USER SET#####
{
  minval<-2
  maxval<-100
  alpha<-0.2 #Lernrate [0,1], je hoeher desto, schneller lernt der Agent, vermischt allerdings auch staerker Erfahrungen aus verschiedenen States miteinander
  resprice<-1
  NNUpdateCycle<-200
}

#####################################################PRESET#####
{
  actionexp.df<-setNames(data.frame(matrix(ncol = 19, nrow = 10000)), c("NPlayers","Round","AgentVal",paste("Maxbid", 2:11,sep=""),"Globalmaxbid","AgentSD","VtEstimator","ActionRel","QReward","NUpdates"))
  vt_estimator.df<-setNames(data.frame(matrix(ncol = 17, nrow = 10000)), c("NPlayers","Round","AgentVal",paste("Maxbid", 2:11,sep=""),"Globalmaxbid","AgentSD","Vtrue","NUpdates"))
  balance.df<-setNames(data.frame(1:11, 0, 0),c("Player","Cash","Times Won"))
  timeystats.mtr<-setNames(data.frame(matrix(0,ncol = 12, nrow = 1)), c("Auction",seq(1,11,1)))
  agentstats.mtr<-setNames(data.frame(matrix(ncol = 5, nrow = 0)), c("Auction","Vt","Maxplayerbid","Agentbid","Winamount"))
  auc_counter<-0
  currdev_vt<-NA
  currdev_action<-NA
  next_epsilon<-NA
}
#####################################################COMPUTATION#####
for(iter in 1:10000){
  
  #Neue Auktion  
  {
    rm(list=ls(pattern="agentbiddetails"))
    rm(list=ls(pattern="tracer"))
    auc_counter<-auc_counter+1
    globalsd<-round(runif(1,min=5,max=10),digits=1)
    n<-round(runif(1,min=2.5,max=11.49),digits=0) #Number of players
    currenv<-defenv.func(minval,maxval,n,globalsd)
    globalmaxbid<-resprice
    lastbids<-rep(0,n) #die letzten Gebote werden wieder auf null gesetzt. lastbids koennte auch currmaxbids heissen
    optout<-rep(F,n)
    bidround<-0
    roundresult<-list(F,0,0,0,rep(F,n))
    for(k in 1:n)assign(paste("tracerp",k,sep = ""),list())
    for(k in 1:n)assign(paste("agentbiddetailsp",k,sep = ""),list(0,0))
  }
  
  while(roundresult[[1]]==F) {
    
    bidround<-bidround+1
    
    newbids<-vector()
    for(k in 1:n){
      agentbiddetails<-agentbid.func(exp.df=actionexp.df,agentval=currenv[[2]][k],agentsd=currenv[[3]][k],player=k)
      newbids<-c(newbids,agentbiddetails[[1]])
      assign(paste("agentbiddetailsp",k,sep = ""),agentbiddetails)
    }
    
    #Auktionator beurteilt
    roundresult<-auctioneer.func(highroller = roundresult[[2]],valtrue = currenv[[1]])
    
    #tracer werden ueberschrieben solange der entsprechende Agent noch drin ist
    for(k in 1:n){
      if(roundresult[[5]][k]==F||as.character(bidround)==1){
        agentbiddetails<-get(paste("agentbiddetailsp",k,sep=""))
        tracer<-get(paste("tracerp",k,sep=""))
        tracer[[bidround]]<-c(agentbiddetails[[4]],agentbiddetails[[2]],agentbiddetails[[3]])
        assign(paste("tracerp",k,sep=""),tracer)
      }
    }
    
    globalmaxbid<-roundresult[[4]]
    optout<-roundresult[[5]]
    lastbids<-roundresult[[6]]
    
    
  } #ENDE AUKTION
  
  
  if(roundresult[[2]]!=0){
    balance.df[roundresult[[2]],3]<-balance.df[roundresult[[2]],3]+1
    balance.df[roundresult[[2]],2]<-balance.df[roundresult[[2]],2]+roundresult[[3]]
  }
  
  for (k in 1:n){
    vt_estimator.df<-vtlearn.func(exp.df=vt_estimator.df,agentval = currenv[[2]][k],player=k)
  }
  
  for (k in 1:n){
  actionexp.df<-actionlearn.func(exp.df=actionexp.df,player=k)
  }
  
  amdata<-match(NA, actionexp.df[,1],nomatch = (nrow(actionexp.df)+1))-1
  
  
  
  
  ################STATISTICS####
  
  #agentstats.mtr[nrow(agentstats.mtr)+1,]<-c(auc_counter,currenv[[1]],round(playersnextbids[[2]],digits=3),roundresult[[6]][1],ifelse(roundresult[[2]]==1,roundresult[[3]],0))
  
  cat("\n","\n","Agent ",roundresult[[2]]," hat in Auktion ",auc_counter,", Runde ",bidround," soeben ",roundresult[[3]]," verwirklicht.",
        "\n","Gebot: ",globalmaxbid, " bei Vtrue ",currenv[[1]],".","\n",sep="")
  
  
  
  if(auc_counter%%100==0){
    
    amdata_vt<-match(NA, vt_estimator.df[,1],nomatch = (nrow(vt_estimator.df)+1))-1
    
    cat("\n","\n","Auktion No.",auc_counter,
        "\n","Gelernte states:",amdata,"| Daten fuer vt:",amdata_vt,
        "\n","Mean Relative Deviation(vt_nn):",currdev_vt,"| Mean Relative Deviation(action_nn):",currdev_action,
        "\n","Letzte Epsilons:",next_epsilon,"\n","Balance:","\n",sep = " ")
    print(t(balance.df[,-1]))
    
    cat("\n","Aktuelle Korrelationen mit Vt:","\n")
    print(cor(vt_estimator.df[1:amdata_vt,-c(16,17)],vt_estimator.df$Vtrue[1:amdata_vt]))
    
    timeystats.mtr<-rbind(timeystats.mtr,c(auc_counter,t(balance.df)[2,]))
    
    plot(timeystats.mtr[,1:2],ylab="Balance (red=Agent)",xlab="Auction",type="l",col="red",
         ylim=c(min(timeystats.mtr[,2:12])-20,max(timeystats.mtr[,2:12])+20))
    for(i in 3:12){
      lines(timeystats.mtr[,c(1,i)],col="black")
    }
    
    #Erfassung von stat. Werten
    #next_epsilon<-calc_epsilon.func(timeystats.mtr)
    next_epsilon<-round(0.51-(0.5-(200/(auc_counter+400))),digits=3)
    
  }
  
  if(auc_counter%%NNUpdateCycle==0){
    
    cat("\n","UPDATING VT NN...","\n")
    
    vt_nn<-create_nn.func(exp.df=vt_estimator.df[,1:16],type="vt_nn")
    
    currdev_vt<-vt_nn[[length(vt_nn)-2]]
    
    cat("\n","DONE! UPDATING V_ESTIMATORS IN ACTION DF","\n")
    
    actionexp.df<-vt_updates.func(exp.df = actionexp.df)
    
    cat("\n","DONE! UPDATING ACTION NN","\n")
    
    action_nn<-create_nn.func(exp.df=subset(actionexp.df[1:amdata,c(1:3,14:18)],actionexp.df$QReward!=0),type="action_nn")
    currdev_action<-action_nn[[length(vt_nn)-2]]
    
    cat("GOING ON...","\n")
  }
}

