

df_patients$openmrs_name <- ""
df_patients$openmrs_lastname <- ""
df_patients$sql_update <- ""


for (i in 1:nrow(df_patients) ) {
  
  
  df_tmp <- get_openmrs_names(df_patients$uuidopenmrs[i],df_patients$mainclinicname[i])
  
  if(nrow(df_tmp)>0){
    
    if(is.na(df_tmp$middle_name[1])){
      f_name <-  df_tmp$given_name[1] 
    }
    else{
      f_name <-  paste0(df_tmp$given_name[1] , " ", df_tmp$middle_name[1] )
    }

    
    df_patients$openmrs_name[i] <- f_name
    l_name <- df_tmp$family_name[1]
    df_patients$openmrs_lastname[i] <- l_name
    uuid_openmrs <- df_patients$uuidopenmrs[i]
    pat_id <- df_patients$id[i] 
    
    df_patients$sql_update[i] <-paste0(" update public.sync_temp_patients set firstnames ='", f_name, "' , lastname ='", l_name, "' where
                                    id = ",pat_id , " and uuidopenmrs = '" , uuid_openmrs ,"' ;")
    
  } else {
    message(df_patients$sp_patientid[i] , " -  Nao foi encontrado")
  }
   

}


### Remover Pacientes sem correspondencia no OpenMRS

df_patients_found. <- df_patients[which(df_patients$openmrs_name!=""),]




for (i in 1:nrow(df_patients_found.) ) {
  
  
  sql <- df_patients_found.$sql_update[i]
  append_to_file("sql_fix_patients_remaining_.txt",text = as.character(sql))
  
}


