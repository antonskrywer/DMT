# zzz.R
#
# Registriert einmalig beim Package-Load einen Basic-Handler fuer das
# 'logging'-Paket. Ohne das ist 'logging' funktionsfaehig, aber output-los:
# logging::loginfo()/logwarn()/logerror() (siehe trial_logic.R, DMT.R,
# results.R, data-raw/stimuli.R) schreiben ohne registrierten Handler
# nirgendwohin, weshalb sie im Shiny-Server-Log bisher NIE ankamen (nur
# lokal in der R-Konsole ueber andere Kanaele sichtbar). basicConfig()
# richtet einen Konsolen-Handler mit Default-Level INFO ein, der auch vom
# Shiny Server erfasst wird.
.onLoad <- function(libname, pkgname) {
  logging::basicConfig()
}
