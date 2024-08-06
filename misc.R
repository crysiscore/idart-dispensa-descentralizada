library(stringr)

# Define the function
# Detecta a presenca de caracters codificados no nome
detect_codes_pat_names <- function(first.name, last.name) {
  # Check if the input string has exactly 6 characters
  
  if (nchar(first.name) == 6 & nchar(last.name) == 6 ){
    
    if( grepl("[0-9]", first.name) |  grepl("[0-9]", last.name) ) {
      return("TRUE")
    } else {
      # check if both first.name or last.name  has got a mix of at least 2 uppercase letters an on lowercase letters
      if( grepl("[A-Z]{2,}", first.name) & grepl("[a-z]", first.name) || grepl("[A-Z]{2,}", last.name) & grepl("[a-z]", last.name)   )
      {
        return("TRUE")
      }
      else {
        return("FALSE")
      }
    }
    
  } else {
    
     return("FALSE")
  }

}


# Define the function
# Coparar dados demograficos e clinicas

compare_pat_demografic_clinic <- function(vec.clinic.uuid, vec.main.clinic.uuid, vec.uuid.openmrs) {
  # Check if the input string has exactly 6 characters
  
  if (vec.clinic.uuid[1] ==vec.clinic.uuid[2]  ) { 
    
    if( vec.main.clinic.uuid[1] == vec.main.clinic.uuid[2] ){
      
      if( (is.na(vec.uuid.openmrs[1]) & !is.na(vec.uuid.openmrs[2]) ) |  (is.na(vec.uuid.openmrs[2]) & !is.na(vec.uuid.openmrs[1]) ) ){
        
        return("dif_uuid")
      } 
      else  if(is.na(vec.uuid.openmrs[1]) & is.na(vec.uuid.openmrs[2]) ) {
        return("igual")
      }
      else if( vec.uuid.openmrs[1] == vec.uuid.openmrs[2] ){
        
        return("igual")
        
      } else { # UUIDs diferentes
      
          return("dif_uuid")
        
      } 
    } else { # US diferentes (pouco provavel)
      
      return("dif_main_clinic")
      
    }
    
    
  }
  else {
    
    
    if( vec.uuid.openmrs[1] == vec.uuid.openmrs[2] ){
      
      return("dif_clinic")
      
    } else {
      
      return("dif_clinic_uuid")
    }
    
 
  }
  
}


# Define the function
# Verifica o uuid corecto do paciente

get_openmrs_uuid <- function(nid, us.name){
  
  uuid <- ""
  
  if(us.name == "CS PORTO"){
    
    uuid <- patients_porto$uuid[ which(str_squish(patients_porto$NID)==nid)]
    
  } else if (us.name =="CS Xipamanine" ){
    
    uuid <- patients_xipamanine$uuid[ which(str_squish(patients_xipamanine$NID)==nid)]
    
  } else if (us.name =="CS ALBASINE" ){
    
    uuid <- patients_albasine$uuid[ which(str_squish(patients_albasine$NID)==nid)]
    
  } else if (us.name == "CS Chamanculo"){
    
    uuid <- patients_chamanculo$uuid[ which(str_squish(patients_chamanculo$NID)==nid)]
    
  } else if (us.name == "CS Alto Mae"){
    
    uuid <- patients_altomae$uuid[ which(str_squish(patients_altomae$NID)==nid)]
    
  } else if (us.name ==  "CS Bagamoio" ){
    
    uuid <- patients_bagamoio$uuid[ which(str_squish(patients_bagamoio$NID)==nid)]
    
  } else if (us.name == "CS 1 de Maio"){
    
    uuid <- patients_1_maio$uuid[ which(str_squish(patients_1_maio$NID)==nid)]
      
  } else if (us.name ==  "Centro de Saude Jose Macamo"){
    
    uuid <- patients_josemacamo_cs$uuid[ which(str_squish(patients_josemacamo_cs$NID)==nid)]
    
  } else if (us.name == "CS Maxaquene" ){
    
    uuid <- patients_maxaquene$uuid[ which(str_squish(patients_maxaquene$NID)==nid)]
    
  } else if (us.name == "Centro de Saude de Zimpeto" ){
    
    uuid <- patients_zimpeto$uuid[ which(str_squish(patients_zimpeto$NID)==nid)]
    
  } else if (us.name == "Centro Saude Polana Canico" ){
    
    uuid <- patients_polana_canico$uuid[ which(str_squish(patients_polana_canico$NID)==nid)]
    
  } else if (us.name == "Centro de Saude da Catembe"){
    
    uuid <- patients_catembe$uuid[ which(str_squish(patients_catembe$NID)==nid)]
  }  
  
  return(uuid)
  
}



create_sql <- function(delete_flag, id, patientid,uuidopenmrs, syncuuid){
  
  if(delete_flag=="TRUE"){
    
    if(is.na(syncuuid)){
      sql_query <-  paste0("delete from sync_temp_patients where id =", id ," and patientid = '", patientid, "' and uuidopenmrs= '", uuidopenmrs , "' and syncuuid is null;");
    } else {
      sql_query <-  paste0("delete from sync_temp_patients where id =", id ," and patientid = '", patientid, "' and uuidopenmrs= '", uuidopenmrs , "' and syncuuid='",  syncuuid, "' ;");
    }
    
    return(sql_query)
  }

}

create_sql_fix_wrong_openmrs_uuid <- function(update_uuid_openmrs, id, patientid,old.uuid.openmrs,new.uuid.openmrs , clinic.name){
  
  sql_query <- ""
  if(update_uuid_openmrs=="FALSE"){
    
      sql_query <-  paste0("update  sync_temp_patients set uuidopenmrs ='", new.uuid.openmrs ,"'  where id = ", id ," and patientid = '", patientid, "' and uuidopenmrs= '", old.uuid.openmrs , "' and clinicname ='", clinic.name ,"' ;");

    
  }
  return(sql_query)
}


append_to_file <- function(file_path, text) {
  # Open the file in append mode
  file_connection <- file(file_path, open = "a")
  
  # Write the text followed by a newline character
  writeLines(text, con = file_connection)
  
  # Close the file connection
  close(file_connection)
}

