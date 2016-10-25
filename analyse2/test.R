logDataLoad = FALSE;

dirs <- list.dirs(path = ".", full.names = TRUE, recursive = FALSE)
player_id <- 0

DTA <- data.table();
DTAAInt <- data.table();
DTAAIntPlayer <- data.table(); #actor actro interaction sur le joueur uniquement
DTAInt <- data.table();
DTAItemInt <- data.table();
DTAPos <- data.table();
DTPlaces <- data.table();
DTForm <- data.table();

if(logDataLoad)
  print("Chargement des donnees: ");

#On parse tous les dirs a la racine
for(d in dirs){
  if(logDataLoad)
    print(d);
  #si on est sur un rep de datas, c'est un nouveau joueur
  if(grepl("data",d)){
    if(logDataLoad)
      print("data");
    player_id <- player_id + 1;
    is_result = TRUE;
    
    #si c'est un rep de data mod ou vanilla, on a les bonnes vars, on load
    if(is_result){
      if(logDataLoad)
        print("Grabbing files...");
      
      #on recup le game mode
      DTModeLoc <- as.data.table(read.csv(paste(d,"/game_type.del",sep="" ),header=FALSE,sep=";"))
      setnames(DTModeLoc, old=c("V1","V2"), new=c("mode", "time"));
      DTModeLoc[, modeName:="vanilla"][mode==0, modeName:="mod"];
      
      DTAIntKindLoc <- as.data.table(read.csv(paste(d,"/actor_interaction_kind.del",sep="" ),header=FALSE,sep=";"))
      setnames(DTAIntKindLoc, old=c("V1","V2"), new=c("action", "actionName"));
      
      DTItemIntKindLoc <- as.data.table(read.csv(paste(d,"/item_interaction_kind.del",sep="" ),header=FALSE,sep=";"))
      setnames(DTItemIntKindLoc, old=c("V1","V2"), new=c("action", "actionName"));
      
      DTALoc <- as.data.table(read.csv(paste(d,"/actor.del",sep="" ),header=FALSE,sep=";"))
      DTALoc$MyPlayerId <- player_id
      setnames(DTALoc, old=c("V1","V2","V3","V4","V5","V6"), new=c("id", "name","kind","X","Y","Z"))
      DTA <- rbind(DTA,DTALoc);
      
      
      
      DTAAIntLoc <- as.data.table(read.csv(paste(d,"/actor_actor_interaction.del",sep="" ),header=FALSE,sep=";"))
      setnames(DTAAIntLoc, old=c("V1","V2","V3","V4","V5","V6","V7","V8"), new=c("actor1", "actor2","V3","X","Y","Z","time","action"))
      DTAAIntLoc$MyPlayerId <- player_id
      DTAAIntLoc[, MyMod:=DTModeLoc[1]$modeName][time>DTModeLoc[2]$time, MyMod:=DTModeLoc[2]$modeName]
      
      #join sur les actions, pour avoir les bon noms d'action
      setkey(DTAIntKindLoc,action)
      setkey(DTAAIntLoc,action)
      DTAAIntLoc <- DTAAIntLoc[DTAIntKindLoc, nomatch=0]
      
      DTAAInt <- rbind(DTAAInt,DTAAIntLoc);
      DTAAIntPlayer <- rbind(DTAAIntPlayer,DTAAIntLoc[actor1==DTALoc[kind==0]$id] );
      DTAAIntPlayer <- rbind(DTAAIntPlayer,DTAAIntLoc[actor2==DTALoc[kind==0]$id] );
      
      DTAIntLoc <- as.data.table(read.csv(paste(d,"/actor_interaction.del",sep="" ),header=FALSE,sep=";"))
      setnames(DTAIntLoc, old=c("V1","V2","V3","V4","V5","V6","V7"), new=c("actor","X","Y","Z","time","action","V7"))
      DTAIntLoc$MyPlayerId <- player_id
      DTAIntLoc[, MyMod:=DTModeLoc[1]$modeName][time>=DTModeLoc[2]$time, MyMod:=DTModeLoc[2]$modeName]
      
      #join sur les actions, pour avoir les bon noms d'action
      setkey(DTAIntKindLoc,action)
      setkey(DTAIntLoc,action)
      DTAIntLoc <- DTAIntLoc[DTAIntKindLoc, nomatch=0]
      
      DTAInt <- rbind(DTAInt,DTAIntLoc);
      
      
      DTAItemIntLoc <- as.data.table(read.csv(paste(d,"/actor_item_interaction.del",sep="" ),header=FALSE,sep=";"))
      setnames(DTAItemIntLoc, old=c("V1","V2","V3","V4","V5","V6","V7"), new=c("actor","item","X","Y","Z","time","action"))
      DTAItemIntLoc$MyPlayerId <- player_id
      DTAItemIntLoc[, MyMod:=DTModeLoc[1]$modeName][time>=DTModeLoc[2]$time, MyMod:=DTModeLoc[2]$modeName]
      
      #join sur les actions, pour avoir les bon noms d'action
      setkey(DTItemIntKindLoc,action)
      setkey(DTAItemIntLoc,action)
      DTAItemIntLoc <- DTAItemIntLoc[DTItemIntKindLoc, nomatch=0]
      
      DTAItemInt <- rbind(DTAItemInt,DTAItemIntLoc);
      
      
      DTAPosLoc <- as.data.table(read.csv(paste(d,"/actor_position.del",sep="" ),header=FALSE,sep=";"))
      setnames(DTAPosLoc, old=c("V1","V2","V3","V4","V5"), new=c("actor","time","X","Y","Z"))
      DTAPosLoc$MyPlayerId <- player_id
      DTAPosLoc[, MyMod:=DTModeLoc[1]$modeName][time>=DTModeLoc[2]$time, MyMod:=DTModeLoc[2]$modeName]
      DTAPos <- rbind(DTAPos,DTAPosLoc);
      
      
      DTPlacesLoc <- as.data.table(read.csv(paste(d,"/place.del",sep="" ),header=FALSE,sep=";"))
      setnames(DTPlacesLoc, old=c("V1","V2","V3","V4"), new=c("id","X","Y","Z"))
      DTPlacesLoc$MyPlayerId <- player_id
      DTPlaces <- rbind(DTPlaces,DTPlacesLoc);
      if(logDataLoad)
        print(paste("add ",nrow(DTPlacesLoc)," places"));
      
      #on recup le formulaire
      form_file <- list.files(path=d,pattern="answers_.*\\csv");
      #on teste si y'a un fichier (impossible de faire un test correct, is null et companie, r me soule)
      if(paste("test",form_file,sep="") != "test"){
        DTFormLoc <- as.data.table(read.csv(paste(d,"/",form_file,sep="" ),header=FALSE,sep=";"))
        #print(DTFormLoc);
        #setnames(DTFormLoc, old=c("V1","V2","V3","V4"), new=c("id","X","Y","Z"))
        DTFormLoc$MyPlayerId <- player_id;
        DTForm <- rbind(DTForm,DTFormLoc);
      }else{
        print(paste(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>  ERROR : FORMULAIRE MANQUANT !!!! in ",d));
      }
      
      if(logDataLoad)
        print("... done");
      
      remove(DTPlacesLoc);
      remove(DTALoc);
      remove(DTAPosLoc);
      remove(DTAItemIntLoc);
      remove(DTAIntLoc);
      remove(DTAAIntLoc);
      remove(DTAIntKindLoc);
      
    }
  }
}

DTPlaces <- as.data.table(DTPlaces)
DTAInt <- as.data.table(DTAInt)
DTAAInt <- as.data.table(DTAAInt)
DTAPos <- as.data.table(DTAPos)