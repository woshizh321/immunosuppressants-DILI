suppressPackageStartupMessages({
  library(data.table)
})

# -----------------------------
# 1. MedDRA ASCII directory
# -----------------------------
MEDDRA_DIR <- "D:/FAERS/MedDRA/MedDRA_28_0_Chinese_1/ascii-280"

# -----------------------------
# 2. Read basic term tables
# -----------------------------
pt   <- fread(file.path(MEDDRA_DIR, "pt.asc"),   sep = "$", header = FALSE, quote = "")
hlt  <- fread(file.path(MEDDRA_DIR, "hlt.asc"),  sep = "$", header = FALSE, quote = "")
hlgt <- fread(file.path(MEDDRA_DIR, "hlgt.asc"), sep = "$", header = FALSE, quote = "")
soc  <- fread(file.path(MEDDRA_DIR, "soc.asc"),  sep = "$", header = FALSE, quote = "")

setnames(pt,   c("pt_code",   "pt_name"),   1:2)
setnames(hlt,  c("hlt_code",  "hlt_name"),  1:2)
setnames(hlgt, c("hlgt_code", "hlgt_name"), 1:2)
setnames(soc,  c("soc_code",  "soc_name"),  1:2)

# -----------------------------
# 3. Read hierarchy relations
# -----------------------------
pt_hlt   <- fread(file.path(MEDDRA_DIR, "pt_hlt.asc"),   sep = "$", header = FALSE)
hlt_hlgt <- fread(file.path(MEDDRA_DIR, "hlt_hlgt.asc"), sep = "$", header = FALSE)
hlgt_soc <- fread(file.path(MEDDRA_DIR, "hlgt_soc.asc"), sep = "$", header = FALSE)

setnames(pt_hlt,   c("pt_code",   "hlt_code"),   1:2)
setnames(hlt_hlgt, c("hlt_code",  "hlgt_code"), 1:2)
setnames(hlgt_soc, c("hlgt_code", "soc_code"),  1:2)

# -----------------------------
# 4. Build PT → SOC full mapping
# -----------------------------
meddra_map <- pt_hlt[
  hlt_hlgt, on = "hlt_code"
][
  hlgt_soc, on = "hlgt_code"
][
  pt, on = "pt_code"
][
  hlt, on = "hlt_code"
][
  hlgt, on = "hlgt_code"
][
  soc, on = "soc_code"
]

# -----------------------------
# 5. Select & order columns
# -----------------------------
meddra_map <- meddra_map[, .(
  pt_code, pt_name,
  hlt_code, hlt_name,
  hlgt_code, hlgt_name,
  soc_code, soc_name
)]

# -----------------------------
# 6. QC checks
# -----------------------------
cat("Total PTs:", uniqueN(pt$pt_code), "\n")
cat("Mapped PTs:", uniqueN(meddra_map$pt_code), "\n")
cat("Unique SOCs:", uniqueN(meddra_map$soc_name), "\n")

# -----------------------------
# 7. Save
# -----------------------------
OUT_DIR <- "99_shared/dict/meddra_hierarchy"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

fwrite(
  meddra_map,
  file.path(OUT_DIR, "pt_hlt_hlgt_soc_v28_0.csv")
)

cat("Saved to:", file.path(OUT_DIR, "pt_hlt_hlgt_soc_v28_0.csv"), "\n")
