

MSSG <- function(language){
  lapply(list(
    downloaded_questionnaire =
      data.frame(
        en = "Downloaded questionnaire",
        pt = "Descarregado questionario",
        es = "Descargado cuestionario"),
    questionnaire =
      data.frame(
        en = "Questionnaire",
        pt = "Questionario",
        es = "Cuestionario"),
    does_not_exist =
      data.frame(
        en = "does not exist",
        pt = "não existe",
        es = "no existe"
      )


     ), function(x) x[1,which(colnames(x) == language)])
}

