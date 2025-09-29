gert::git_branch_create("feat/analise-cepal") 
gert::git_branch_checkout("feat/analise-cepal")

daily_git <- function(msg = "update") {
  stopifnot(nchar(msg) > 0)
  try(gert::git_pull(rebase = TRUE), silent = TRUE)
  gert::git_add(".")
  if (nrow(gert::git_status()) == 0L) {
    message("Nada para commitar.")
    return(invisible(TRUE))
  }
  gert::git_commit(msg)
  try(gert::git_pull(rebase = TRUE), silent = TRUE)
  gert::git_push()
  message("✅ Pull → Commit → Push concluído.")
}
gert::git_pull(rebase = TRUE)


install.packages("gert")
library(gert)

daily_git <- function(msg = "update") {
  stopifnot(nchar(msg) > 0)
  try(gert::git_pull(rebase = TRUE), silent = TRUE)
  gert::git_add(".")
  if (nrow(gert::git_status()) == 0L) {
    message("Nada para commitar.")
    return(invisible(TRUE))
  }
  gert::git_commit(msg)
  try(gert::git_pull(rebase = TRUE), silent = TRUE)
  gert::git_push()
  message("✅ Pull → Commit → Push concluído.")
}
install.packages(c("renv","usethis","gitcreds"))

library(gert)
library(renv)

# sempre traga as mudanças remotas antes de começar
gert::git_pull(rebase = TRUE)

# entre na sua branch de trabalho (troque o nome ao lado):
branch <- "feat/analise-cepal"
if (!(branch %in% gert::git_branch_list()$name)) {
  gert::git_branch_create(branch)
}
gert::git_branch_checkout(branch)

# veja o que mudou
gert::git_status()

# selecione o que vai neste commit (adicione caminhos ou "." para tudo)
gert::git_add(c("R/", "outputs/", "scripts/cepal_biblio.R"))

# faça o commit com mensagem clara
gert::git_commit("feat: adiciona análise anual WoS/Scopus e relatório HTML")

# sincronize antes de enviar (resolve conflitos cedo, se houver)
gert::git_pull(rebase = TRUE)

# envie para o remoto
gert::git_push()

start_day <- function(branch = "feat/analise-cepal") {
  try(gert::git_pull(rebase = TRUE), silent = TRUE)
  if (!(branch %in% gert::git_branch_list()$name)) gert::git_branch_create(branch)
  gert::git_branch_checkout(branch)
  message("✅ Pull feito e na branch: ", branch)
}

save_block <- function(msg, paths = c(".")) {
  gert::git_add(paths)
  if (nrow(gert::git_status()) == 0L) { message("Nada para commitar."); return(invisible(TRUE)) }
  gert::git_commit(msg)
  try(gert::git_pull(rebase = TRUE), silent = TRUE)
  gert::git_push()
  message("✅ Commit/Pull/Push: ", msg)
}

end_day <- function(msg = "chore: fechamento do dia") {
  save_block(msg, paths = ".")
}

start_day()
