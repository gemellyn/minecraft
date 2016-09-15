dirs <- list.dirs(path = ".", full.names = TRUE, recursive = FALSE)
player_id <- 0
player_mod <- "modunknown"

DTA <- data.table();
DTAAInt <- data.table();
DTAInt <- data.table();
DTAItemInt <- data.table();
DTAPos <- data.table();

#On parse tous les dirs a la racine
for(d in dirs){
  print(d);
  #si on est sur un rep de datas, c'est un nouveau joueur
  if(grepl("data",d)){
    print("data");
    player_id <- player_id + 1;
    dirsPlayer <- list.dirs(path = d, full.names = TRUE, recursive = FALSE)
    #dans ce rep, on a des sous rep vanilla et mod avec les datas
    for(dp in dirsPlayer){
      print(dp);
      is_result = FALSE;
      if(grepl("vanilla",dp)){
        player_mod = "vanilla"
        is_result = TRUE;
      }
      if(grepl("mod",dp)){
        player_mod = "mod"
        is_result = TRUE;
      }
      
      #si c'est un rep de data mod ou vanilla, on a les bonnes vars, on load
      if(is_result){
        print("Grabbing files...");
        DTALoc <- as.data.table(read.csv(paste(dp,"/actor.del",sep="" ),header=FALSE,sep=";"))
        DTALoc$MyPlayerId <- player_id
        DTALoc$MyMod <- player_mod
        DTA <- rbind(DTA,DTALoc);
        remove(DTALoc);
        
        DTAAIntLoc <- as.data.table(read.csv(paste(dp,"/actor_actor_interaction.del",sep="" ),header=FALSE,sep=";"))
        DTAAIntLoc$MyPlayerId <- player_id
        DTAAIntLoc$MyMod <- player_mod
        DTAAInt <- rbind(DTAAInt,DTAAIntLoc);
        remove(DTAAIntLoc);
        
        DTAIntLoc <- as.data.table(read.csv(paste(dp,"/actor_interaction.del",sep="" ),header=FALSE,sep=";"))
        DTAIntLoc$MyPlayerId <- player_id
        DTAIntLoc$MyMod <- player_mod
        DTAInt <- rbind(DTAInt,DTAIntLoc);
        remove(DTAIntLoc);
        
        DTAItemIntLoc <- as.data.table(read.csv(paste(dp,"/actor_item_interaction.del",sep="" ),header=FALSE,sep=";"))
        DTAItemIntLoc$MyPlayerId <- player_id
        DTAItemIntLoc$MyMod <- player_mod
        DTAItemInt <- rbind(DTAItemInt,DTAItemIntLoc);
        remove(DTAItemIntLoc);
        
        DTAPosLoc <- as.data.table(read.csv(paste(dp,"/actor_position.del",sep="" ),header=FALSE,sep=";"))
        DTAPosLoc$MyPlayerId <- player_id
        DTAPosLoc$MyMod <- player_mod
        DTAPos <- rbind(DTAPos,DTAPosLoc);
        remove(DTAPosLoc);
        
        print("... done");
      }
    }
  }
}
