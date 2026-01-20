# Skapa GIS-fil med punktlager

Detta repository innehåller en Shinyapplikation (`karta_skapa_punktlager`) för Samhällsanalys, Region Dalarna.

## Struktur

- All appkod ligger i katalogen `app/`
  - `ui.R`, `server.R`, `global.R`
  - `www/` för favicon och övriga statiska filer
  - `R/` för hjälpfunktioner

- Deployment sker via GitHub Actions (`.github/workflows/deploy.yml`)
  till Shiny-servern (appmapp `/srv/shiny-server/karta_skapa_punktlager`).

Justera detta README efter behov.
