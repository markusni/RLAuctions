# # 'Gen2!
# Kein Eligibility Trace, alle realisierte States werden neu angelegt

#####################################################FUNCTIONS
#rm(list=ls()) #Clean-Up!
library(digest)

{
defenv.func<-function(minval,maxval,n,sd){
  
  valtrue<-round(runif(1,min = minval,max = maxval),digits = 5)
  sdcurr<-abs(round(rnorm(n,mean=0,sd=sd),digits = 1))
  vals<-abs(round(rnorm(n,mean=valtrue,sd=sdcurr),digits=1))
  
  return(list(valtrue,vals,sdcurr))
}

defnextnaivebid.func<-function(n,highroller,lastbids,globalmaxbid,val,sd,minbidinc,realprofits){
  lastbids<-lastbids[-1]
  #val<-currenv[[2]][-1]
  #sd<-currenv[[3]][-1]
  highroller<-highroller-1
  
  nextminbid<-globalmaxbid+minbidinc
  playermaxbid<-val-(qnorm(realprofits^(1/n)))*sd
  nextbids<-round(((playermaxbid-nextminbid)*runif((n-1),min = 0.1,max = 0.4)+nextminbid),digits=1)
  
  optout<-which(nextbids<nextminbid)
  nextbids[optout]<-lastbids[optout]
  nextbids[highroller]<-lastbids[highroller]
  return(nextbids)
}

similarity1.func<-function(exp.df, agentval, agentsd, amdata){
  
  tempmatrix<-matrix(c(exp.df$AgentVal[1:amdata],exp.df$AgentSD[1:amdata]), ncol = 2)
  
  lowerframe<-(qnorm((1-alpha)/2,mean = agentval,sd = agentsd))
  upperframe<-qnorm((1-alpha)/2+alpha,mean = agentval,sd = agentsd)
  
  sims<-matrix(c(qnorm((1-alpha)/2,mean = tempmatrix[,1],sd = tempmatrix[,2]),
                 qnorm((1-alpha)/2+alpha,mean = tempmatrix[,1],sd = tempmatrix[,2])),ncol=2)
  
  sims<-matrix(c(sims[,1]/upperframe-1,lowerframe/sims[,2]-1),ncol=2)
  sims<-matrix(sims[,1]*sims[,2])
  
  return(which(sims>0))
  
}

similarity2.func<-function(tempmatrix, currstate){ #Beim hier gewaehlten Verfahren (aehnelt Standardabweichung) werden gro?e Abweichungen h?her gewichtet als kleine
  
  if(nrow(tempmatrix)==0){
    return(vector())
  }
  
  currstate[currstate==0]<-0.01 #Hier muss aufgrund des Teilungsfehlers durch 0 ein Wert eingesetzt werden
  tempmatrix[tempmatrix==0]<-0.01

  sims<-as.matrix(tempmatrix)%*%diag(1/currstate)   #Werte der Matrix werden durch currstate Werte dividiert
  sims<-sqrt(rowSums((sims-1)^2)/ncol(tempmatrix))
  
  return(which(sims<alpha))
}

learnstateagent.func<-function(exp.df,lastbids, agentval,agentsd, agentbiddetails){
  
  lastbidsps<-sort(lastbids[-1])
  
  amdata<-match(NA, exp.df[,1],nomatch = (nrow(exp.df)+1))-1
  
  #Bei Anfangsfällen (alle 0) reicht prevexp2 als Anhaltspunkt. Gibt es hier mehr als 1 ähnliche Erfahrungen, wird die aktuelle nicht angelegt
  if(length(agentbiddetails[[2]])>=1 && all(lastbids==0)){
    return(list(F))
  }
  
  #Suche nach dem genau gleichen Statecase in der Erfahrung
  currhash<-digest(round(c(agentval,agentsd,lastbidsps,agentbiddetails[[1]]),digits=10))
  
  if(match(currhash,exp.df$Statehash[1:amdata],nomatch = 0)==0){
    newstate<-c(agentval,agentsd,lastbidsps,agentbiddetails[[1]],0,0)
    return(list(T,(amdata+1),currhash,newstate))
  }else{
    return(list(F))
  }
}

learnexpagent.func<-function(exp.df,eligibility,roundresult){
  #exp.df<-agentexp.df
  #trace.df<-eligibilitytrace.df
  
  if(length(eligibility[[2]])==0){
    return(exp.df)
  }
  
  if(roundresult[[2]]==1){
    resagent<-roundresult[[3]]
  }else{
    resagent<-0
  }
  
  startcol<-which(colnames(exp.df)=="AgentVal")
  
  #Direkte Nebenliegende Aktionen werden gefunden(+- 0.05), out of range werden entfernt
  neighboractions<-round(c(eligibility[[1]]-0.05,eligibility[[1]]+0.05),digits = 2)
  neighboractions<-neighboractions[neighboractions>0 & neighboractions<=1.25]
  
  #Die Tabelle mit relevanten States wird pro aktion 1x wiederholt
  temp.mtr<-do.call("rbind", replicate(length(neighboractions)+1, exp.df[eligibility[[2]],startcol:(startcol+n)], simplify = FALSE))

  #...und mit den aktionen kombiniert
  temp.mtr<-cbind(temp.mtr,rep(c(eligibility[[1]],neighboractions),each=length(eligibility[[2]])))
  
  hashs<-vector()                                     #Hashwerte fuer die beoachteten State/Action Kombos finden
  for(rows in 1:nrow(temp.mtr)){
    newhash<-digest(round(as.numeric(temp.mtr[rows,]),digits = 10))
    hashs<-c(hashs,newhash)
  }
  
  #Wurde eine State/Action Kombo mehrmals besucht, so gilt nur der letzte Fall
  drop<-which(rev(match(hashs[length(hashs):1],hashs[length(hashs):1])-(1:length(hashs)))!=0)

  if(length(drop)>0){
    temp.mtr<-temp.mtr[-drop,]
    hashs<-hashs[-drop]
  }
    
  payouts<-round(resagent/((2-alpha)^(abs(temp.mtr[,ncol(temp.mtr)]-eligibility[[1]])/0.05)),digits=2)
  
  temp.mtr<-cbind(temp.mtr,payouts)

  amdata<-match(NA, exp.df[,1],nomatch = (nrow(exp.df)+1))-1      #amdata im exp.df
  
  posstates<-match(hashs,exp.df[1:amdata,1])               #Aufenthaltsorte bzw.fehlende States identifizieren
  
  newstates<-which(is.na(posstates))
  
  if(length(newstates)>0){
    exp.df[(amdata+1):(amdata+length(newstates)),1]<-hashs[newstates]             #Neue States anlegen
    exp.df[(amdata+1):(amdata+length(newstates)),-1]<-c(temp.mtr[newstates,],1)
  }
  
  oldstates<-posstates[!is.na(posstates)]

  #Alte States werden lernend ergaenzt
  if(length(oldstates)>0){
   
    #Erhoehen Sample-Zaehler 
    exp.df$Samples[oldstates]<-exp.df$Samples[oldstates]+1
    
    #Erweitern d ExpR
    exp.df$ExpectedR[oldstates]<-round(exp.df$ExpectedR[oldstates]+(1/exp.df$Samples[oldstates])*
      (temp.mtr[which(!is.na(posstates)),ncol(temp.mtr)]-exp.df$ExpectedR[oldstates]),digits=2)
      
      # Sutton Barto(167):
      # Wertalt+(1/AnzahlSamples)*(wertneu-wertalt)
  }
  
  return(exp.df)
}

defbidagent.func<-function(exp.df,agentval,agentsd,lastbids,globalmaxbid,minbidinc,optout,bidreason,eligibility){
  
  lastaction<-eligibility[[1]]
  
  certainty<-F
  
  #Im Fall sd=0 muss getrickst werden. Auch sollte der agent dann niemals hoeher bieten als seine val
  if(agentsd==0){
    certainty<-T
    agentsd<-0.1
  }
  
  #lastbids<-round(runif(10,min=1,max=20),digits=1)
  lastbidsps<-sort(lastbids[-1])

  #Auffinden der Anzahl an beschriebenen Zeilen in exp.df
  amdata<-match(NA, exp.df[,1],nomatch = (nrow(exp.df)+1))-1
  
  #Suche nach aehnlichen Erfahrungen fuer diesen Statecase
  #Zunaechst im Hinblick auf val und sd
  prevexp1<-similarity1.func(exp.df = exp.df, agentval = agentval, agentsd = agentsd, amdata = amdata)
  
  #Dann im Hinblick auf die maxbids
  startcol<-which(colnames(exp.df)=="currmaxbidP2")
  prevexp2<-prevexp1[similarity2.func(tempmatrix = exp.df[prevexp1,startcol:(startcol+n-2)],currstate=lastbidsps)]
  
  #Wenn der Agent eine Valuierung von 0 hat so waehlt er immer die Aktion 0 (Multiplikator wuerde nichts beeinflussen)
  if(as.character(agentval)==0){
    return(list(0,prevexp2,"NOT REAL 0"))
  }
  
  #Wenn der Agent das max Gebot inne hat so muss er dieses nicht erhoehen
  if(lastbids[1]>0 && as.character(lastbids[1])==globalmaxbid){
    return(list(lastaction,prevexp2,bidreason))
  }
  
  #Wenn der Agent schon optout ist bietet er stets sein letztes Gebot als max
  if(optout==T){
    return(list(lastaction,prevexp2,"NOT REAL 1"))
  }
  
  #Bei Mangel an Erfahrung wird die options.mtr kuenstlich angelegt mit der sicheren Erfahrung fuer 0
  if(length(prevexp2)==0){
    options.mtr<-matrix(0,ncol=2)
    
  }else{
    #Alle Erfahrungen mitteln
    options.mtr<-as.matrix(aggregate(exp.df$ExpectedR[prevexp2]~exp.df$Action[prevexp2],FUN=mean))
    options.mtr[,2]<-round(options.mtr[,2],digits=2) #Runden, damit kleine Unterschiede keine grosse Rolle bekommen
  
  }

  #Fehlende Erfahrungen ergaenzen
  zerooptions<-seq(0,1.25,0.05)[-match(as.character(options.mtr[,1]),as.character(seq(0,1.25,0.05)))]
  
  options.mtr<-rbind(options.mtr,matrix(c(zerooptions,rep(0,length(zerooptions))),ncol=2))
  
  #Bei Sicherheit sollte der Agent kein Gebot über seiner val abgeben
  if(certainty==T){
    options.mtr<-options.mtr[-which(options.mtr[,1]>1),]
  }
  
  #Ausschliessen unmoeglicher Gebote (aufgrund von Mindestinkrement und geltendem Maxbid)
  nextminaction<-ceiling((globalmaxbid+minbidinc)/agentval*100)/100
  options.mtr<-rbind(options.mtr[-which(options.mtr[,1]<nextminaction),],c(lastaction,0)) #die letzte Aktion wird als mögliche Option hinzugefügt und repräsentiert die Entscheidung auszusteigen, mit sicherem Erwartungswert 0
  
  #Check ob lastaction nun die einzig verbleibende Moeglichkeit ist (Entspricht keiner Erhoehung)
  if(nrow(options.mtr)==1){
    return(list(lastaction,prevexp2,"NOT REAL 2"))
  }
    
  #Exploit Actions identifizieren
  maxexpraction<-which(as.character(options.mtr[,2])==as.character(max(options.mtr[,2])))
  bidreason<-unname(options.mtr[maxexpraction,1])
  
  #Falls kein maxi/minimum existiert
  if(length(maxexpraction)==nrow(options.mtr)){
    action<-round(sample(options.mtr[,1],1),digits=2)
    bidreason<-c("Zufallswahl",action,"aus",options.mtr[,1])
    
    if(nrow(options.mtr)==length(seq(0,1.25,0.05))){
      bidreason<-paste("Keinerlei Erfahrungen,",action,sep = " ")
    }
    return(list(unname(action),prevexp2,bidreason))
  }
  
  #Standardfall: Action definieren
  
  if(round(runif(1,min=0,max=1/epsilon),digits=0)==1){  #EXPLORE!
    cat("\n","EXPLORATION IN PROGRESS")
    bidreason<-"EXPLORE <4"
    
    #Stehen genug Möglichkeiten zur Verfügung so soll er nur die Vielversprechendsten explorieren
    if(length(options.mtr[-maxexpraction,1])>=4){
      options.mtr<-options.mtr[-maxexpraction,]
      action<-round(sample(options.mtr[order(-options.mtr[,2]),1][1:4],1),digits=2)
      return(list(unname(action),prevexp2,paste("Exploring Handpicked",action,sep = " ")))
    }
    
    #Sonst:
    if(length(options.mtr[-maxexpraction,1])>1){
      action<-sample(options.mtr[-maxexpraction,1],1)
    }else{
      action<-options.mtr[-maxexpraction,1]
    }
    
  }else{                                                #EXPLOIT!
    if(length(maxexpraction)>1){
      action<-sample(options.mtr[maxexpraction,1],1)
    }else{
      action<-options.mtr[maxexpraction,1]
    }
      
  }

  action<-round(action,digits=2)
  return(list(unname(action),prevexp2,bidreason))
}

auctioneer.func<-function(n, highroller, valtrue, lastbids, newbids, globalmaxbid, minbidinc,optout){
  

  newbids[which(optout==T)]<-lastbids[which(optout==T)]
  newbids[newbids<lastbids]<-lastbids[newbids<lastbids]
  
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
    optout[highroller]<-F                     #Highroller darf in der Folgerunde auf Gebotserhöhung verzichten
    highroller<-which.max(newbids)
    return(list(F,highroller,0,globalmaxbid,optout,newbids))
  }
  
}

writeauctionlog.func<-function(){
  
  amdata<-match(NA, auctionlog.df[,1],nomatch = (nrow(auctionlog.df)+1))-1
  auctionlog.df[amdata+1,1:ncol(auctionlog.df)]<-c(round(c(i,currenv[[1]],bidround,lastbids,roundresult[[2]],roundresult[[3]]),digits=2),optbid)
  return(auctionlog.df)
  
}

defoptbid<-function(){
  maxplayerbid<-max(currenv[[2]][-1]-(qnorm(realprofits^(1/n)))*currenv[[3]][-1])
  bids<-currenv[[2]][1]*seq(0.05,1.25,0.05)
  if(length(bids[bids>(maxplayerbid-minbidinc+0.01) & bids<currenv[[1]]])==0){
    return(NA)
  }
  optbid<-round(min(bids[bids>(maxplayerbid-minbidinc+0.01) & bids<currenv[[1]]]),digits=1)
  return(optbid)
}

}


#####################################################USER SET // UNVERAENDERLICH
{
minval<-1
maxval<-41
globalsd<-3
n<-11 #Spieleranzahl, ergibt 1 Agenten und n-1 Mitspieler
iterator<-15000
minbidinc<-4
alpha<-0.2 #Lernrate [0,1], je hoeher desto, schneller lernt der Agent, vermischt allerdings auch staerker Erfahrungen aus verschiedenen States miteinander
epsilon<-0.025
resprice<-0.1
realprofits<-0.6    #Anteil der Auktionen in denen Profite realisierbar wären
}
#####################################################SYSTEM SET

{
auctionlog.df<-setNames(data.frame(matrix(ncol = 6+n, nrow = iterator*5)),c("Auction No.","vtrue","Biddind Round No.",paste("BidP", 1:n,sep = ""),"Winner/Highroller","Winamount","optibidAgent"))
balance.df<-setNames(data.frame(1:n, 0, 0),c("Player","Cash","Times Won"))
agentexp.df<-setNames(data.frame(matrix(ncol = 5+n, nrow = iterator*10)), c("Statehash","AgentVal","AgentSD",paste("currmaxbidP", 2:n,sep=""),"Action","ExpectedR","Samples"))
timeystats.mtr<-matrix(c(0,0,0),ncol=3,nrow=1)
}

#####################################################START COMPUTING

#Auktionsschleife
{
#newcash<-0
#cashinc<-0
optprofit<-0
actualprofit<-0
wincounter<-0
optcounter<-0
i<-0
}

#for(i in 10001:iterator){
while(i%%100!=0 || balance.df[1,2]<=max(balance.df[-1,2])){

  start.time <- Sys.time()
  #Neue Auktion wird aufgespannt
  {
    i<-i+1
    currenv<-defenv.func(minval,maxval,n,globalsd)
    globalmaxbid<-resprice
    lastbids<-rep(0,n) #die letzten Gebote werden wieder auf null gesetzt. lastbids könnte auch currmaxbids heißen
    playersnextbids<-rep(0,n-1)
    optout<-rep(F,10)
    bidround<-0
    roundresult<-list(F,vector())
    bidreason<-vector()
    eligibility<-list(0,vector(),vector())
  }

  #Neue Bietrunde

  while(roundresult[[1]]==F) {

    bidround<-bidround+1
    
    optbid<-defoptbid()
                                                                                          
    #Player entscheiden
    playersnextbids<-defnextnaivebid.func(n,highroller = roundresult[[2]],lastbids,globalmaxbid,val=currenv[[2]][-1],
                                        sd=currenv[[3]][-1],minbidinc, realprofits)
    
    #Agent entscheidet aufgrund von existierender Erfahrung
    agentbiddetails<-defbidagent.func(exp.df = agentexp.df,agentval = currenv[[2]][1],agentsd = currenv[[3]][1],
                                      lastbids,globalmaxbid,minbidinc,optout = optout[1],bidreason,eligibility)
    

    #Bids werden gesammelt
    newbids<-c(round(agentbiddetails[[1]]*currenv[[2]][1],digits=1),playersnextbids)
    
    #Auktionator beurteilt
    roundresult<-auctioneer.func(n,highroller = roundresult[[2]],valtrue = currenv[[1]],lastbids,newbids,globalmaxbid,minbidinc,optout)
    
    globalmaxbid<-roundresult[[4]]
    optout<-roundresult[[5]]
    
    #Tracer wird ueberschrieben wenn Auktion noch nicht vorbei ist und der Agent noch drin ist
    if(roundresult[[5]][1]==F){
      #Agent legt fuer die neue Aktion u.U. neuen State an falls er ihn noch nicht kennt und das Gebot nicht 0 ist
      learnstate<-learnstateagent.func(exp.df = agentexp.df,lastbids,agentval = currenv[[2]][1],
                                       agentsd = currenv[[3]][1],agentbiddetails)
      
       if(learnstate[[1]]==T){
        agentexp.df[learnstate[[2]],1]<-learnstate[3]
        agentexp.df[learnstate[[2]],2:ncol(agentexp.df)]<-learnstate[[4]]
        agentbiddetails[[2]]<-c(agentbiddetails[[2]],learnstate[[2]])
      }
    
    eligibility<-agentbiddetails
    bidreason<-agentbiddetails[[3]]
    }
    
    amdata<-match(NA, agentexp.df[,1],nomatch = (nrow(agentexp.df)+1))-1
    
    
    #Ende Bietrunde
    lastbids<-roundresult[[6]]
  
    auctionlog.df<-writeauctionlog.func()
  
  }#Ende Auktion
  

  if(roundresult[[2]]>0){
  balance.df[roundresult[[2]],3]<-balance.df[roundresult[[2]],3]+1
  balance.df[roundresult[[2]],2]<-balance.df[roundresult[[2]],2]+roundresult[[3]]
  }
  
  agentexp.df<-learnexpagent.func(exp.df = agentexp.df,eligibility,roundresult)
  amdata<-match(NA, agentexp.df[,1],nomatch = (nrow(agentexp.df)+1))-1
  
  #Ende ANPASSUNGEN
  maxplayerbid<-round(max(currenv[[2]][-1]-(qnorm(realprofits^(1/n)))*currenv[[3]][-1]),digits=4)
  
  
  if(!is.na(optbid)){
    optprofit<-optprofit+round((currenv[[1]]-optbid),digits = 2)
  }
  

  if(as.character(roundresult[[2]])==1){
    actualprofit<-actualprofit+roundresult[[3]]
    wincounter<-wincounter+1
    
    if(!is.na(optbid)&&as.character(globalmaxbid)==optbid){
     optcounter<-optcounter+1 
    }
    
    cat("\n","\n","Der Agent hat in Auktion ",i,", Runde ",bidround," soeben ",roundresult[[3]]," verwirklicht.",
        "\n","Gebot: ",globalmaxbid, " bei Vt ",currenv[[1]]," aus Grund ",paste(shQuote(bidreason), collapse=", "),
        "\n","Moeglich waeren optimiert ",round((currenv[[1]]-optbid),digits = 2)," bei Gebot ",optbid,
        "\n (Maxpbid ",maxplayerbid," vs. VAgent ",currenv[[2]][1],")",sep="")
  }
  
  
  #Statusmeldung
  if(i%%100==0){
    lap.time<-round(as.numeric(Sys.time() - start.time),digits = 3)
    cat("\n","\n","Auktion No.",i,"Letzte dauerten",lap.time,"min.","Balance:","\n",balance.df[1:n,1],"\n",
        balance.df[1:n,2],"\n","Gelernte states:",amdata,
        "\n", "Gewinn", actualprofit,"aus moeglichen",optprofit,"(",round(actualprofit/optprofit*100,digits=1),"%).",
        "Perfekt waren",round(optcounter/wincounter*100, digits=1),"% der winning bids")
    start.time <- Sys.time()
    #cashinc<-balance.df$Cash[1]-newcash
    #newcash<-balance.df$Cash[1]
    
    timeystats.mtr<-rbind(timeystats.mtr,c(i,balance.df$Cash[1],round(optcounter/wincounter*100, digits=1)))
    plot(timeystats.mtr[,1:2])
    
    optprofit<-0
    actualprofit<-0
    optcounter<-0
    wincounter<-0
    
  }

}

plot(timeystats.mtr[,1],timeystats.mtr[,3])

#test.df<-subset(agentexp.df,agentexp.df$currmaxbidP11==0)
#testold.df<-subset(agentexpold.df,agentexpold.df$currmaxbidP11==0)

#Ideen: Regression, diskontieren nebenliegender Aktionen, Runden bei den Geboten?

library(openxlsx)

write.xlsx(agentexp.df[1:amdata,],"agentexp.xlsx")
write.xlsx(timeystats.mtr,"timeystats.xlsx")
?openxlsx
agentexpold.df<-read.xlsx("agentexp.xlsx",sheet=1,colNames=T)
nrow(subset(agentexp.df,agentexp.df[,13]==0))

plot(agentexp.df$Action,agentexp.df$ExpectedR)
