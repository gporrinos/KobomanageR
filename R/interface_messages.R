

interface_messages <- function(language){
  lapply(list(
    drive_authentication_TITLE =
      data.frame(
        en = "Google Drive Authentication",
        pt = "Iniciar sessão no Google Drive",
        es = "Iniciar sesión en Google Drive",
        fr = "Se connecter à Google Drive"),
    enter_your_google_email =
      data.frame(
        en = "Enter your Google email:",
        pt = "Introduza o email:",
        es = "Introduzca su correo electrónico:",
        fr = "Entrez votre adresse e-mail :"),
    authentication_unsuccessful =
      data.frame(
        en = "Authentication unsuccessful, please authenticate again",
        pt = "Autenticação falhou, por favor tente novamente",
        es = "Autenticación fallida, por favor intente de nuevo",
        fr = "Authentification échouée, veuillez réessayer"),
    drive_authenticated_as =
      data.frame(
        en = "Authenticated in Google drive as",
        pt = "Iniciou sessão no Google drive como",
        es = "Sesión iniciada en Google Drive como",
        fr = "Connecté à Google Drive en tant que"),
    continue =
      data.frame(
        en = "Continue?",
        pt = "Continuar?",
        es = "¿Continuar?",
        fr = "Continuer ?"),
    yes =
      data.frame(
        en = "Yes",
        pt = "Sim",
        es = "Sí",
        fr = "Oui"),
    no =
      data.frame(
        en = "No",
        pt = "Não",
        es = "No",
        fr = "Non"),
    kobotoolbox_authentication_TITLE =
      data.frame(
        en = "Kobotoolbox Authentication",
        pt = "Iniciar sessão no Kobotoolbox",
        es = "Iniciar sesión en Kobotoolbox",
        fr = "Se connecter à Kobotoolbox"),
    input_kobo_url =
      data.frame(
        en = "Kobotoolbox URL (e.g., eu.kobotoolbox.org)",
        pt = "Endereço do servidor do KoboToolbox (ex. eu.kobotoolbox.org)",
        es = "Dirección del servidor de KoboToolbox (ej. eu.kobotoolbox.org)",
        fr = "Adresse du serveur KoboToolbox (ex. eu.kobotoolbox.org)"),
    kobo_username =
      data.frame(
        en = "Kobotoolbox username",
        pt = "Usuário do Kobotoolbox",
        es = "Usuario de Kobotoolbox",
        fr = "Nom d'utilisateur Kobotoolbox"),
    kobo_password =
      data.frame(
        en = "KoBo password",
        pt = "Palavra passe",
        es = "Contraseña",
        fr = "Mot de passe"),
    logged_in_kobotoolbox_as =
      data.frame(
        en = "Logged in Kobotoolbox as",
        pt = "Iniciou sessão em Kobotoolbox como",
        es = "Sesión iniciada en Kobotoolbox como",
        fr = "Connecté à Kobotoolbox en tant que"),
    kobo_login_failed =
      data.frame(
        en = "Kobotoolbox login failed. Please check credentials.",
        pt = "O seu login no KoboToolbox falhou. Por favor, verifique as credenciais",
        es = "El inicio de sesión en KoboToolbox falló. Por favor, verifique las credenciales.",
        fr = "La connexion à KoboToolbox a échoué. Veuillez vérifier vos identifiants."),
    kobo_login =
      data.frame(
        en = "Login",
        pt = "Iniciar sessão",
        es = "Iniciar sesión",
        fr = "Se connecter"),
    accessing_kobo =
      data.frame(
        en = "Accessing Kobotoolbox server",
        pt = "Acessando o servidor do Kobo Toolbox",
        es = "Accediendo al servidor de KoboToolbox",
        fr = "Accès au serveur KoboToolbox"),
    downloading_questionnaire =
      data.frame(
        en = "Downloading questionnaire",
        pt = "Descarregando questionário",
        es = "Descargando cuestionario",
        fr = "Téléchargement du questionnaire"),
    processing_data =
      data.frame(
        en = "Processing data",
        pt = "Processando dados de",
        es = "Procesando datos de",
        fr = "Traitement des données de"),
    enter_your_zenodo_token =
      data.frame(
        en = "Please introduce your Zenodo token",
        pt = "Por favor introduza o seu token do zenodo",
        es = "Por favor introduzca su token de Zenodo",
        fr = "Veuillez entrer votre token Zenodo"),
    zenodo_authentication_TITLE =
      data.frame(
        en = "ZENODO authentication",
        pt = "Iniciar sessão no Zenodo",
        es = "Iniciar sesión en Zenodo",
        fr = "Se connecter à Zenodo"),
    invalid_token =
      data.frame(
        en = "Invalid token, please verify credentials",
        pt = "Token não valido, por favor verificar informações",
        es = "Token no válido, por favor verifique las credenciales",
        fr = "Token invalide, veuillez vérifier vos identifiants"),
    using_zenodo_token =
      data.frame(
        en = "Using the following zenodo token:",
        pt = "Usando o seguinte token do zenodo:",
        es = "Usando el siguiente token de Zenodo:",
        fr = "Utilisation du token Zenodo suivant :"),
    download_new_data =
      data.frame(
        en = "Download new data",
        pt = "Descarregar dados novos",
        es = "Descargar datos nuevos",
        fr = "Télécharger les nouvelles données"),
    collate_new_data =
      data.frame(
        en = "Collate new data to local database",
        pt = "Colar dados novos à base de dados local",
        es = "Agregar datos nuevos a la base de datos local",
        fr = "Ajouter les nouvelles données à la base de données locale"),
    update_data_from_local =
      data.frame(
        en = "Update database from local database",
        pt = "Atualizar base de dados desde a base de dados local",
        es = "Actualizar base de datos desde la base de datos local",
        fr = "Mettre à jour la base de données depuis la base de données locale"),
    delete_processed_instances =
      data.frame(
        en = "Delete from server already-processed data",
        pt = "Apagar do KoboToolbox os dados que já foram descarregados",
        es = "Eliminar del servidor los datos ya procesados",
        fr = "Supprimer du serveur les données déjà traitées"),
    estimate_catch =
      data.frame(
        en = "Estimate catch",
        pt = "Estimar capturas",
        es = "Estimar capturas",
        fr = "Estimer les captures")
  ), function(x) x[1, which(colnames(x) == language)])
}
