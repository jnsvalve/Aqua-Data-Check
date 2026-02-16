# Universal Aquaculture Data Harvest & Harmonisation

A reproducible R workflow for collecting, harmonising, and comparing European aquaculture production data (tonnes & EUR) from **Eurostat**, **FAO**, and **EUMOFA**.

## Purpose

This project provides a unified pipeline that loads or downloads raw aquaculture datasets, standardises their structures, converts units and currencies, and produces comparable **country–year production volumes** and **production values** across three major international data sources.

The goal is to offer a coherent, transparent, and reproducible basis for analysing freshwater aquaculture in Europe.

---

## Data Sources

### Eurostat (`fish_aq2a`)
- Annual aquaculture production
- Units: **TLW** (tonnes live weight) and **EUR**
- Freshwater identified via: `aquaenv == "FRW"`

### FAO (FishStatJ CSV exports)
- Aquaculture quantity (`Q_tlw`) in **tonnes**
- Aquaculture value (`V_USD_1000`) in **thousand USD**
- Freshwater identified via: environment code `IN`
- Value is scaled (`× 1000`) and converted USD → EUR via FX table

### EUMOFA (Yearly Aquaculture CSV)
- Volume: `volume_kg` → tonnes
- Value in EUR: `value_eur`
- Freshwater rows identified with keyword matching in `commodity_group`

---

## Key Features

### ✔ Harmonisation Layer
- Converts all sources into a consistent long-format table
- Standardises country codes (FAO UN‑codes → ISO2)
- Aggregates to `(country × year × source × measure)`
- Ensures correct units:
  - **tonnes** for production  
  - **euros** for value  

### ✔ Freshwater Filter
Unified freshwater extraction:
- Eurostat → `FRW`
- FAO → `IN`
- EUMOFA → pattern match `"freshwater"` or `"inland"`

### ✔ FX Conversion
- Automatically loads `usd_eur_rates.csv` (if available)
- Fallback FX rates included for 2018–2024
- Applies USD → EUR conversion for FAO values

### ✔ Comparison & Diagnostics
- ES/FAO and ES/EUMOFA ratios for both tonnes and euros
- Log-difference metrics
- Flagging of large divergences
- Optional kg↔tonne unit‑sanity checks
- Consistency report exported as CSV

### ✔ Output Files
Automatically produced:
- `Europe_summary.xlsx` — all aquaculture
- `Europe_freshwater_summary.xlsx` — freshwater only
- `output/freshwater_consistency_YYYYMMDD_HHMM.csv` — diagnostics table
