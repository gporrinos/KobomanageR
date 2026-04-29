

#' @export
KobomanageR_msg <- function(language){
  msg <-
    list(
      accessing_kobotoolbox_server =
        list(
          en = "Accessing KoboToolbox server",
          pt = "Accessando o servidor do Kobotoolbox",
          es = "Accediendo al servidor de Kobotoolbox"
        ),
      downloading_new_submissions =
        list(
          en = "Downloading new submissions",
          pt = "Descarregando novas entradas de dados questionários",
          es = "Descargando nuevos datos de los formularios"
        ),
      Processing =
        list(
          en = "Processing",
          pt = "Processando",
          es = "Procesando"
        ),
      Processing_new_submissions =
        list(
          en = "Processing new submissions",
          pt = "Processando novas entradas de dados",
          es = "Procesando nuevos datos"
        ),
      no_new_submissions =
        list(
          en = "There are no new submissions",
          pt = "Não existem novas entradas de dados",
          es = "No existen nuevas entradas de datos"
        ),
      Processing_new_attachments =
        list(
          en = "Processing new attachments",
          pt = "Processando novos anexos",
          es = "Procesando nuevos adjuntos"
        ),
      No_new_attachments =
        list(
          en = "There are no new attachments",
          pt = "Não existem novos anexos",
          es = "No hay nuevos adjuntos"
        ),
      Downloading =
        list(
          en = "Downloading",
          pt = "Descarregando",
          es = "Descargando"
        ),
      Processing =
        list(
          en = "Processing",
          pt = "Processando",
          es = "Procesando"
        ),
      new_attachments_process_may_take_a_while =  # Part of sentence "Downloading n new attachments"
        list(
          en = "new attachments. The process may take a while.",
          pt = "anexos novos. O processo pode demorar.",
          es = "nuevos adjuntos. El proceso puede tardar."
        ),
      downloading_attachment =            # Part of sentence "processing attachment i of n"
        list(
          en = "Downloading attachment",
          pt = "Descarregando anexo",
          es = "Descargando adjunto"
        ),

      of =            # Part of sentence "processing attachment i of n"
        list(
          en = "of",
          pt = "de",
          es = "de"
        ),
      publishing_attachment_repo =
        list(
          en = "Publishing attachment repository",
          pt = "Publicando repositório de anexos",
          es = "Publicando repositorio de adjuntos"
        ),
      publishing_data_repo =
        list(
          en = "Publishing data repository",
          pt = "Publicando repositório de dados",
          es = "Publicando repositorio de datos"
        )

    )
  return(lapply(msg, function(x) x[[which(names(x) == language)]]))
}
