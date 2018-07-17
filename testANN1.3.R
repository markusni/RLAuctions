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
    playermaxbid<-val-(qnorm(realprofits^(1/n)))*sd
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
    
    lastbids[lastbids==0]<-0.1
    lastbids<-sort(lastbids[-1],decreasing = T)
    
    currstate<-c(n,bidround,minbidinc,agentval,agentsd,lastbids,rep(0,11-n),globalmaxbid)
    
    lastaction<-agentbiddetails[[2]]
    
    #Wenn der Agent Hoechstbieter ist, erhöht er nicht sein eigenes Gebot, falls er schon raus ist, braucht er auch nichts zu kalkulieren
    if(as.character(roundresult[[2]])==1||roundresult[[5]][1]==T){
      return(list(round(lastaction*agentval,digits=1),lastaction,explore,currstate,"Epsilon n/a[1]"))
    }
    
    #Sonst:
    nextminaction<-(globalmaxbid+minbidinc)/agentval
    if(nextminaction>1.9)return(list(round(lastaction*agentval,digits=1),lastaction,explore,currstate,"Epsilon n/a[2]"))
    #amdata<-match(NA, exp.df[,1],nomatch = (nrow(agentexp.df)+1))-1
    
    #Zunächst muss es Ausgangsdaten geben
    if(auc_counter<=2000){
      ifelse(nextminaction>=(0.95+bidround*0.05),action<-lastaction,action<-round(runif(1,min=nextminaction,max=(0.95+bidround*0.05)),digits=2))
      return(list(round(action*agentval,digits=1),action,explore,currstate,"Epsilon n/a[data]")) #Vector() ist Platzhalter für zusätzliche Werte
    }
    
    #Maximieren von reward: E(r)=arg max action, f(features,action): E´(r)=delr/delaction, mittels iteration Netzwerk
    
    greedyaction<-maximizeq.func(nn=currnn, currstate=currstate, maxs=unlist(currnn[[length(currnn)-1]]), mins=unlist(currnn[[length(currnn)]]), nextminaction, lastaction)
    action<-greedyaction
    
    #Set EPSILON
    #dumamdata<-c(4000,4000,20,20)
    #dumreldev<-c(100,5,100,5)
    
    #results<-1/((dumamdata+50*sqrt(dumreldev))/(50*sqrt(dumreldev)+(250/(dumamdata+250))*dumamdata))
    #results<-(50*sqrt(dumreldev)+(250/(dumamdata+250))*dumamdata)/(dumamdata+50*sqrt(dumreldev))
    #round(matrix(c(0,dumreldev[1:2],dumamdata[1],results[1:2],dumamdata[3],results[3:4]),ncol=3,byrow=T),digits=3)
    
    epsilon<-round((8*currdev+(10*amdata)/(10+amdata)) / (15*(2000*currdev)^(1/4)+amdata)+0.01,digits = 3)
    if(epsilon>0.5)epsilon<-0.5
    
    if(runif(1,min=0,max=1000)<=epsilon*1000){
      explore<-1
      print("EXPLORING ACTION")
      #Ist greedy = last action und es soll exploriert werden, werden als Nachbarstates die ersten wieder gültigen States (ab nextminaction) herangenommen
      if(as.character(greedyaction)==lastaction)greedyaction<-(ceiling(nextminaction*100)/100)-0.01
      exploreopts<-seq(greedyaction-0.1,greedyaction+0.1,0.01)[-11]
      exploreopts<-exploreopts[exploreopts>=nextminaction]
      action<-ifelse(length(exploreopts)>1,sample(exploreopts,1),exploreopts)
    }
    
    return(list(round(action*agentval,digits=1),action,explore,currstate,epsilon))
    
  }

  actionlearn.func<-function(exp.df){
    #exp.df<-agentexp.df
    
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
    
    for(i in 1:tracer_l){
      
      amdata<-match(NA, exp.df[,1],nomatch = (nrow(exp.df)+1))-1
      
      update<-tracer[[tracer_l-i+1]]
      
      explore<-update[18]
      
      #newtarget<-resagent/(1.5-(alpha/2))^(i-1)
      
      #Suche nach Vorerfahrungen für die Updates
      relevantstates<-which(exp.df$Nplayers==update[1])
      startcol<-which(colnames(exp.df)=="Round")
      similarities<-similar.func(tempmatrix = exp.df[relevantstates,c(startcol:(startcol+n+2),startcol+14,startcol+15)],currstate=update[c(2:(n+4),16,17)])
      relevantstates<-relevantstates[similarities[[1]]]
      
      
      #Alte States werden angepasst
      if(as.character(length(relevantstates))>=1){
        
        #Falls im Folgenden exploriert wurde, duerfen die Actionvalues nur nach oben angepasst werden
        if(explorepath==T){
          similarities[[2]]<-similarities[[2]][which(exp.df$Result[relevantstates]<newtarget)]
          relevantstates<-relevantstates[which(exp.df$Result[relevantstates]<newtarget)]
        }
        
        m<-(log(5)-log(10))/((log(1.3)*0.4)-0.6*log(1.3))
        k<-(log(10)+(log(1.3)*0.4)*m)/log(1.3)
        
        
        #Anpassung der nun uebrigen States
        exp.df$Result[relevantstates]<-exp.df$Result[relevantstates]+round((1/((similarities[[2]]+1)^(k-alpha*m)))*(newtarget-exp.df$Result[relevantstates]),digits=3)
        exp.df$NUpdates[relevantstates]<-exp.df$NUpdates[relevantstates]+1
        
        #Neue States angelegt mit willkuerlichem Grenzwert fuer Aehnlichkeit 0.02
        if(any(similarities[[2]]<=0.02)==F){
          update[18]<-round(alpha*newtarget,digits=3)
          exp.df[amdata+1,]<-c(update,1)
        }
      }else{
        #Falls kein State bekannt ist wird er neu angelegt
        update[18]<-round(alpha*newtarget,digits=3)
        exp.df[amdata+1,]<-c(update,1)
      }
      
      #Updates gehen nur bis einschließlich einer Stelle wo exploriert wurde
      
    
      
      if(as.character(explore)==1)explorepath<-T
      
    }
    return(exp.df)
    
  }
  
  vtlearn.func<-function(exp.df,agentval){
    
    if(tracer_l==0){
      return(exp.df)
    }
    
    lastaction<-agentbiddetails[[2]]
    
    if(as.character(roundresult[[2]])==1){
      vtrue<-lastaction*agentval+roundresult[[3]]
    }else{
      return(exp.df)
    }
    
    tracer_l<-length(tracer)
    
    c("Round","AgentVal",paste("Maxbid", 2:11,sep=""),"Globalmaxbid","Vt")

    amdata<-match(NA, exp.df[,1],nomatch = (nrow(exp.df)+1))-1
        
    for(i in tracer_l){
      update<-tracer[[tracer_l-i+1]]
      update<-c(update[c(0,1,2)],vtrue)
      
      exp.df[amdata+1,]<-update
    }
    
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
  
  maximizeq.func<-function(nn, currstate, maxs, mins, nextminaction, lastaction){
    
    minaction<-mins[names(mins)=="Action"]
    maxaction<-maxs[names(mins)=="Action"]
    
    mins<-mins[1:(length(mins)-2)]
    maxs<-maxs[1:(length(maxs)-2)]
    #(1-0.91)/(1.11-0.91)
    
    currstate.scaled<-as.data.frame(scale(t(as.data.frame(currstate)),center = mins, scale = maxs - mins))
    currstate.scaled<-cbind(currstate.scaled,0)
    
    nextminaction_c<-ceiling(nextminaction*10)/10
    
    possactions<-c(lastaction,seq(nextminaction_c,2,0.1))
    
    
    expres<-vector()
    
    for(i in 1:length(possactions)){
      
      scaledaction<-as.vector(scale(possactions[i],center=minaction,scale=maxaction-minaction))
      
      currstate.scaled[length(currstate.scaled)]<-scaledaction
      
      expres[i]<-as.vector(compute(nn,currstate.scaled)$net.result)
      
    }
    
    if(as.character(which.max(expres))==1){
      return(lastaction)
    }
    
    possactions<-seq(possactions[which.max(expres)]-0.09,possactions[which.max(expres)]+0.09,0.01)
    possactions<-possactions[possactions>nextminaction]
    
    expres<-vector()
    
    for(i in 1:length(possactions)){
      
      scaledaction<-as.vector(scale(possactions[i],center=minaction,scale=maxaction-minaction))
      
      currstate.scaled[length(currstate.scaled)]<-scaledaction
      
      expres[i]<-as.vector(compute(nn,currstate.scaled)$net.result)
      
    }
    
    return(possactions[which.max(expres)])
    
  }
  
  create_nn.func<-function(exp.df){
    
    #amdata<-match(NA, exp.df[,1],nomatch = (nrow(exp.df)+1))-1
    
    #Lösungsmechanismus: Neurales Netz
    exp.df<-exp.df[1:amdata,1:(ncol(exp.df)-1)]
    exp.df$Result<-round(exp.df$Result,digits=1)
    exp.df<-exp.df[which(exp.df$Result<=5&exp.df$Result>=-5),]
    
    index<-sample(1:amdata,round(0.8*amdata))
    
    maxs<-apply(exp.df, 2, max) 
    mins<-apply(exp.df, 2, min)
    
    scaledexp.df<-as.data.frame(scale(exp.df, center = mins, scale = maxs - mins))
    
    training.df<-scaledexp.df[index,]
    testing.df<-scaledexp.df[-index,]
    
    cols<-names(exp.df)
    formula<-as.formula(paste("Result ~", paste(cols[!cols %in% "Result"], collapse = " + ")))
    newnn<-neuralnet(formula,data=training.df,hidden=c(5,3),linear.output=T)
    
    #Voerhersagung um sd zu schätzen
    predictions_new<-compute(newnn,testing.df[,1:(ncol(testing.df)-1)])
    
    predictions_new<-predictions_new$net.result*(max(exp.df$Result)-min(exp.df$Result))+min(exp.df$Result)
    
    realv<-exp.df$Result[-index]
    
    modelsd_new <- sqrt(sum((realv - predictions_new)^2)/length(realv))
    
    realv[realv==0]<-0.01
    meanreldev_new<-sqrt(sum(((as.vector(predictions_new)/realv)-1)^2)/length(realv))
    
    newnn[[length(newnn)+1]]<-meanreldev_new
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
}
###########################################################
{
actionexp.df<-setNames(data.frame(matrix(ncol = 9, nrow = 10000)), c("Nplayers","Round","Minbidinc","AgentSD","VtEstimator","ActionRel","ActionAbs","Result","NUpdates"))
vt_estimator.df<-setNames(data.frame(matrix(ncol = 14, nrow = 10000)), c("Round","AgentVal",paste("Maxbid", 2:11,sep=""),"Globalmaxbid","Vt"))
balance.df<-setNames(data.frame(1:11, 0, 0),c("Player","Cash","Times Won"))
timeystats.mtr<-setNames(t(data.frame(rep(0,12))), c("Auction",seq(1,11,1)))
auc_counter<-0
currdev<-NA
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
    #realprofits<-round(4*(runif(1,min=0.1,max=1)-0.5)^3+0.5,digits=2)    #Anteil der Auktionen in denen Profite realisierbar wÃ¤ren
    realprofits<-0.5
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
    
    agentbiddetails<-agentbid.func(exp.df=agentexp.df,agentval=currenv[[2]][1],agentsd=currenv[[3]][1])
    
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
        "\n","Gebot: ",globalmaxbid, " bei Vt ",currenv[[1]],".","\n",sep="")
    
  }
  
  if(auc_counter%%100==0){
    
    cat("\n","\n","Auktion No.",auc_counter,
        "\n","Gelernte states:",amdata,"\n","Mean Relative Deviation(nn):",currdev,
        "\n","Letztes Epsilon:",curreps,"\n","Balance:","\n",sep = " ")
    print(t(balance.df[,-1]))
    print(cor(agentexp.df[1:amdata,-c(18,19)],agentexp.df$Result[1:amdata]))
    
    timeystats.mtr<-rbind(timeystats.mtr,c(auc_counter,t(balance.df)[2,]))
    
    plot(timeystats.mtr[,1:2],ylab="Balance")
  }

  if(auc_counter%%500==0){
    
    cat("\n","UPDATING NN...","\n")
    
    currnn<-create_nn.func(exp.df = agentexp.df)
    currdev<-currnn[[length(currnn)-2]]
    
    plot(currnn,rep="best")
    cat("GOING ON...","\n")
  }
}

plot(currnn,rep = "best")

#amdata nur einmal deklarieren!



plot(exp.df$Action,exp.df$Result)

cor(agentexp.df[1:amdata,-c(18,19)],agentexp.df$Result[1:amdata])

realv.df<-subset(agentexp.df,agentexp.df$Result!=0)
realv.df$Result<-realv.df$Action*realv.df$AgentVal+realv.df$Result

cors<-cor(realv.df[1:amdata,-c(18,19)],realv.df$Result[1:amdata])
labels(cors)[[1]][which(cors<(-0.2)|cors>0.2)]

test.df<-agentexp.df
amdata<-match(NA, realv.df[,1],nomatch = (nrow(realv.df)+1))-1
test.df<-test.df[1:amdata,]
nrow(test.df)

colnames(test.df)[18]<-"Vt"
nrow(test.df)
test.df<-subset(test.df,test.df$Vt!=0)
test.df$Vt<-test.df$AgentVal*test.df$Action+test.df$Vt
cor(test.df[,-ncol(test.df)],test.df$Vt)
exp.df<-test.df
