

MSSG <- function(language){
  lapply(list(
    submissions_downloaded =
      data.frame(
        en = "Submissions downloaded",
        pt = "Submissões descarregadas",
        es = "Envíos descargados"),
    from = 
      data.frame(
        en = "from",
        pt = "do",
        es = "del"
      ),
    questionnaire =
      data.frame(
        en = "Form",
        pt = "Questionário",
        es = "Formulario"),
    does_not_exist =
      data.frame(
        en = "does not exist",
        pt = "não existe",
        es = "no existe"
      ),
    missing_assetid_and_name = 
      data.frame(
        en = "Missing required parameters: please provide either assetid or form name.",
        pt = "Parâmetros obrigatórios ausentes: forneça o assetid ou o nome do formulário ('name').",
        es = "Faltan parámetros obligatorios: proporcione el assetid o el nombre del formulario ('name')."
      ),
    missing_id_and_uuid = 
      data.frame(
        en = "Missing required parameters: please provide either submission_id or submission_uuid.",
        pt = "Parâmetros obrigatórios ausentes: forneça o submission_id ou o submission_uuid.",
        es = "Faltan parámetros obligatorios: proporcione el submission_id o submission_uuid."
      ),
    Submission = 
      data.frame(
        en = "Submission",
        pt = "Submissão",
        es = "Envio"
      ),
    not_found = 
      data.frame(
        en = "not found",
        pt = "não encontrado",
        es = "no encontrado"
      ),
    deleted_successfully = 
      data.frame(
        en = "deleted successfully",
        pt = "apagada com sucesso",
        es = "eliminada con éxito"
      ),
    deletion_of = 
      data.frame(
        en = "Deletion of",
        pt = "Exclusão de",
        es = "Eliminación de"
      ),
    
    unauthorised = 
      data.frame(
        en = "Unauthorised. Check url and credentials",
        pt = "Não autorizada. Verifique url e credenciais",
        es = "No autorizada. Verifique url y credenciales"
      )
      


     ), function(x) x[1,which(colnames(x) == language)])
}

