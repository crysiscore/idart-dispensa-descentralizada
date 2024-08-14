library(plyr)
source(file = 'param.R')
source(file = 'misc.R')


df_patients <- dbGetQuery(con,query_patients)
df_dispense <- dbGetQuery(con,query_dispense)
df_episode <- dbGetQuery(con,query_episode)

# df_patients <- dbGetQuery(conn = con,statement = "SELECT 
#             sp.id,
#             sp.firstnames as sp_patientfirstname,
#             sp.lastname as sp_patientlastname,
#             sp.patientid as sp_patientid,
#             sp.uuidopenmrs,
#             sp.mainclinicname
#             FROM  public.sync_temp_patients sp
#            
#             where LENGTH(firstnames)=6 and LENGTH(lastname)=6 and  (firstnames ~ '[0-9]' or lastname ~ '[0-9]') ;")

# Pacientes duplicados por NID
df_dup_patients  <- df_patients[duplicated(df_patients) | duplicated(df_patients$patientid, fromLast = TRUE), ]
df_dup_patients = subset(df_patients, df_patients$patientid %in% df_dup_patients$patientid ,)
  
# Add flags do dataframe
df_dup_patients$coded_names <- ""
df_dup_patients$delete_flag <- ""
df_dup_patients$uuid_flag <- ""
df_dup_patients$sites_flag <- ""
df_dup_patients$uuid_openmrs <- ""
df_dup_patients$jsonpresc_flag <- is.na(df_dup_patients$jsonprescribeddrugs)

## Detecta a presenca de caracteres codificados no nome
df_dup_patients$coded_names  <- mapply(df_dup_patients$firstnames,df_dup_patients$lastname, FUN =  detect_codes_pat_names)


## Compara o UUID do openMRS com o UUID do iDART Central
df_dup_patients$uuid_openmrs  <- mapply(df_dup_patients$patientid ,df_dup_patients$mainclinicname, FUN =  get_openmrs_uuid)
df_dup_patients$update_uuid_openmrs <- mapply(df_dup_patients$uuid_openmrs ,df_dup_patients$uuidopenmrs, FUN = function(x,y) x==y  )
df_dup_patients$update_uuid_openmrs <- as.character(df_dup_patients$update_uuid_openmrs)

df_dup_patients$coded_names   <- mapply(df_dup_patients$firstnames,df_dup_patients$lastname, FUN =  detect_codes_pat_names)

#Inicializacao de vectores e contadores
vec_unique_nids <- unique(df_dup_patients$patientid)
vec_nids_triplicados <- c()
df_ambos_duplicados <- data.frame()
load(file = "df_patients_corrected.RData")
df_triplicados <- df_patients_corrected
counter_triplicado <-0
counter_fluxo_inesperado <-0
counter_todos_codificados <- 0
counter_resolvidos <- 0


for (i in 1:length(vec_unique_nids)) {
  
  nid <- vec_unique_nids[i]
  tmp_df  <- subset(df_dup_patients, df_dup_patients$patientid ==nid,)
  counter_nids <- nrow(tmp_df)
  
  
  # Verifica se e duplicado ou triplicado
  
  if(counter_nids==2){
       

    tmp_df$coded_names   <- mapply(tmp_df$firstnames,tmp_df$lastname, FUN =  detect_codes_pat_names)
    tmp_df$sites_flag[1]   <- compare_pat_demografic_clinic( vec.clinic.uuid = tmp_df$clinicuuid,
                                                             vec.main.clinic.uuid = tmp_df$mainclinicuuid, 
                                                             vec.uuid.openmrs = tmp_df$uuidopenmrs)
    tmp_df$sites_flag[2]  <-  tmp_df$sites_flag[1]
    index <- which(tmp_df$coded_names==TRUE)
    other_index = 1;

    
    if(length(index) ==1){
       
      if(index > other_index){
        other_index <- 1
      } else {
        other_index = 2;
      }
       
      if(   is.na(tmp_df$jsonprescribeddrugs[index])  &  is.na(tmp_df$jsonprescribeddrugs[other_index])   ) { 
         # Ambos pacientes nao  tem info da prescricao? 
         # Marcar o paciente com nome codificado para apagar  |   # Marcar o paciente com nome codificado para apagar, ambos pacientes nao tem info da prescricao
          tmp_df$delete_flag[index] <- TRUE
          df_patients_corrected <- rbind.fill(df_patients_corrected, tmp_df)
          counter_resolvidos <- counter_resolvidos + 1
          # break
            
      } else if(  ! is.na(tmp_df$jsonprescribeddrugs[index])  &  ! is.na(tmp_df$jsonprescribeddrugs[other_index])   ) { 
        # Ambos pacientes   tem info da prescricao: Provavelmente informacao igual
        tmp_df$delete_flag[index] <- TRUE
        df_patients_corrected <- rbind.fill(df_patients_corrected, tmp_df)
        counter_resolvidos <- counter_resolvidos + 1
        # break
        
      } else if(  is.na(tmp_df$jsonprescribeddrugs[index])  &  ! is.na(tmp_df$jsonprescribeddrugs[other_index])   ) { 
        # O paciente com o nome correcto tem info. da prescricao
        tmp_df$delete_flag[index] <- TRUE
        df_patients_corrected <- rbind.fill(df_patients_corrected, tmp_df)
        counter_resolvidos <- counter_resolvidos + 1
        # break
        
      } else if ( ! is.na(tmp_df$jsonprescribeddrugs[index])  &&  is.na(tmp_df$jsonprescribeddrugs[other_index])  ) { # O Paciente com o nome  codificado tem info da prescricao?
       
         # Copiar info da prescricao do paciente codificado para o outro paciente
        tmp_df$jsonprescribeddrugs[other_index] <-  tmp_df$jsonprescribeddrugs[index] 
        tmp_df$prescriptiondate[other_index] <-  tmp_df$prescriptiondate[index] 
        tmp_df$regimenome[other_index] <-  tmp_df$regimenome[index] 
        tmp_df$duration[other_index] <-  tmp_df$duration[index] 
        tmp_df$linhanome[other_index] <-  tmp_df$linhanome[index] 
        tmp_df$dispensatrimestral[other_index] <-  tmp_df$dispensatrimestral[index] 
        tmp_df$prescriptionid[other_index] <-  tmp_df$prescriptionid[index] 
        tmp_df$delete_flag[index] <- TRUE
        df_patients_corrected <- rbind.fill(df_patients_corrected, tmp_df) 
        counter_resolvidos <- counter_resolvidos +1
         #break
       
      } else {
        # Fluxo inesperado
        message(tmp_df$patientid[index] , " " ,  tmp_df$firstnames[index] , " Fluxo Inesperado")
        counter_fluxo_inesperado <- counter_fluxo_inesperado +1
      }
       
      
    }
    else { 
      # Ambos pacientes com nomes codificados
      message( tmp_df$patientid[1] , " " ,  tmp_df$firstnames[1] , " Ambos pacientes com nomes codificados")
      counter_todos_codificados <- counter_todos_codificados +1
      df_ambos_duplicados <- rbind.fill(df_ambos_duplicados, tmp_df)
      }
    
    
  }
  else if(counter_nids==3){
      #message( tmp_df$patientid[index] , " " ,  tmp_df$firstnames[index] , " Paciente com NID triplicado")
      counter_triplicado <- counter_triplicado + 1
      vec_nids_triplicados <- c(vec_nids_triplicados,tmp_df$patientid[index])
      
      
      # Remove the patient with coded name
      index <- which(tmp_df$coded_names==TRUE)
      tmp_df$delete_flag[index] <- TRUE
      
      # df_patients_corrected <- rbind.fill(df_patients_corrected, tmåp_df)
      # tmp_df_duplicado  <- subset(tmp_df, tmp_df$coded_names ==FALSE,)
      
      # From remaining rows check which one doesnt have jsonprecribedflags
      index_json_pf <- which(tmp_df$jsonpresc_flag==TRUE)
      
      
     if(length(index_json_pf)==0){
       
       index_no_presc_enddate <- which(is.na(tmp_df$prescriptionenddate))
       
       if(length(index_no_presc_enddate)==1){
         
         tmp_df$delete_flag[index_no_presc_enddate] <-  TRUE
         counter_resolvidos <- counter_resolvidos +1
         
       } else {
         
         index_no_presc_date <- which(is.na(tmp_df$prescriptiondate))
         tmp_df$delete_flag[index_no_presc_date] <-  TRUE
         counter_resolvidos <- counter_resolvidos +1
       }
       
      }
      else if(length(index_json_pf)==1){
        tmp_df$delete_flag[index_json_pf] <-  TRUE
        counter_resolvidos <- counter_resolvidos +1
      } 
     
      
      df_triplicados <- rbind.fill(df_triplicados, tmp_df)
      df_patients_corrected <- rbind.fill(df_patients_corrected, tmp_df) 
      
  } 
  else {
    message("Do Nothing")
  }
  

}

# Create a mark_delete flag in df_ambos_duplicados using a vapply with a function to check wich rows have both variables firstnames and lastname with char length of 6
df_ambos_duplicados$mark_delete <-  mapply( df_ambos_duplicados$firstnames,df_ambos_duplicados$lastname,FUN = function(x,y) {return( nchar(x)==nchar(y) && nchar(x)==6 )} )
df_ambos_duplicados$mark_delete <- as.logical(df_ambos_duplicados$mark_delete )
# Criar sql  para remover duplicados
df_patients_corrected$sql_remove_dups <- mapply( df_patients_corrected$delete_flag,df_patients_corrected$id,df_patients_corrected$patientid,
                                     df_patients_corrected$uuidopenmrs,df_patients_corrected$syncuuid,FUN = create_sql )
df_patients_corrected$sql_update_uuid <-  mapply( df_patients_corrected$update_uuid_openmrs,df_patients_corrected$id,df_patients_corrected$patientid,
                                                  df_patients_corrected$uuidopenmrs,df_patients_corrected$uuid_openmrs,df_patients_corrected$clinicname,FUN = create_sql_fix_wrong_openmrs_uuid )

df_patients_corrected$sql_remove_dups <- as.character(df_patients_corrected$sql_remove_dups )
df_patients_corrected$sql_update_uuid <- as.character(df_patients_corrected$sql_update_uuid )



# Filtra Pacientes da Farmac Mocambique
df_filtered_mocambique_fix_dups <- df_patients_corrected %>% filter(sql_remove_dups != "NULL"  &  clinicuuid=='a42fa142-cb62-4bef-8414-37e5bbe5f0bf')  %>% 
  select(c("id","patientid","firstnames","lastname", "clinicname","mainclinicname" , "uuidopenmrs","uuid_openmrs", 
           "update_uuid_openmrs","coded_names","delete_flag", "update_uuid_openmrs", "sql_remove_dups","sql_update_uuid") )



# Pacientes com uuid do openmrs incorrecto
df_filtered_mocambique_fix_uuid <-  df_patients_corrected %>% filter(sql_update_uuid != ""  &  clinicuuid=='a42fa142-cb62-4bef-8414-37e5bbe5f0bf'  & delete_flag=='FALSE')  %>% 
  select(c("id","patientid","firstnames","lastname", "clinicname","mainclinicname" , "uuidopenmrs","uuid_openmrs", "update_uuid_openmrs","coded_names","delete_flag", "update_uuid_openmrs", "sql_remove_dups","sql_update_uuid") )


## Patients with incorrect uuid
# a <- df_patients_corrected %>% filter((update_uuid_openmrs==FALSE | is.na(update_uuid_openmrs) )  & delete_flag=="")  %>% 
#                          select(c("patientid","firstnames","lastname", "uuidopenmrs","uuid_openmrs", "prescriptiondate","coded_names",
#                                   "update_uuid_openmrs","delete_flag", "sites_flag") )

#a = df_patients_corrected[which(),c("patientid","firstnames","lastname", "uuidopenmrs", "prescriptiondate","coded_names", "uuid_openmrs", "uuid_openmrs", "update_uuid_openmrs","delete_flag", "sites_flag")]



for (i in 1:nrow(b) ) {
  
  
   sql <- b$sql_remove_dups[i]
   
   if(as.character(sql) != "NULL"){
     
     append_to_file("sql_fix_duplicates.txt",text = as.character(sql))
     
   }

   
  
}


