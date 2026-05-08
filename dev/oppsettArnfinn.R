sship::dec(
  "c://Users/ast046/Downloads/kvarus1371ab41b.sql.gz__20260506_093906.tar.gz",
  keyfile = "c://Users/ast046/.ssh/id_rsa",
  target_dir = "c://Users/ast046/Downloads/")

devtools::install("../rapbase/.", dependencies = FALSE)
devtools::install(dependencies = FALSE)

source("dev/sysSetenv.R")
kvarus::run_app(browser = TRUE)
