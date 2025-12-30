# ============================================================
# Immunosuppressants drug classes (FAERS + JADER)
# Version: v5.1 (validated, frozen)
# Scope: Calcineurin inhibitors / mTOR inhibitors /
#        Antiproliferative agents / Corticosteroids / Biologics
# ============================================================

drug_classes <- list(
  
  # ----------------------------------------------------------
  # 1. Calcineurin inhibitors (CNI)
  # ----------------------------------------------------------
  "Calcineurin inhibitors" = c(
    # English / INN
    "TACROLIMUS", "FK506", "PROGRAF", "ADVAGRAF",
    "CYCLOSPORINE", "CYCLOSPORIN", "NEORAL", "SANDIMMUNE",
    
    # Japanese
    "タクロリムス", "プログラフ", "アドバグラフ",
    "シクロスポリン", "ネオーラル", "サンディミューン"
  ),
  
  # ----------------------------------------------------------
  # 2. mTOR inhibitors
  # ----------------------------------------------------------
  "mTOR inhibitors" = c(
    # English / INN
    "EVEROLIMUS", "AFINITOR", "CERTICAN",
    "SIROLIMUS", "RAPAMUNE",
    "TEMSIROLIMUS", "TORISEL",
    
    # Japanese
    "エベロリムス", "アフィニトール", "サーティカン",
    "シロリムス", "ラパミューン",
    "テムシロリムス", "トーリセル"
  ),
  
  # ----------------------------------------------------------
  # 3. Antiproliferative agents (APA)
  # ----------------------------------------------------------
  "Antiproliferative agents" = c(
    # English / INN
    "MYCOPHENOLATE MOFETIL", "MYCOPHENOLIC ACID",
    "CELLCEPT", "MYFORTIC",
    "AZATHIOPRINE", "IMURAN",
    "MIZORIBINE",
    
    # Japanese
    "ミコフェノール酸", "ミコフェノール酸モフェチル",
    "セルセプト", "マイフォルティック",
    "アザチオプリン", "イムラン",
    "ミゾリビン"
  ),
  
  # ----------------------------------------------------------
  # 4. Corticosteroids (systemic)
  # ----------------------------------------------------------
  "Corticosteroids" = c(
    # English / INN
    "PREDNISOLONE", "PREDNISONE",
    "METHYLPREDNISOLONE", "SOLU-MEDROL", "MEDROL",
    "DEXAMETHASONE",
    "HYDROCORTISONE",
    "BETAMETHASONE",
    "TRIAMCINOLONE",
    
    # Japanese
    "プレドニゾロン", "プレドニン", "プレドニゾン",
    "メチルプレドニゾロン", "ソルメドロール",
    "デキサメタゾン", "リンデロン",
    "ヒドロコルチゾン", "コートリル", "コルテフ",
    "ベタメタゾン",
    "トリアムシノロン", "ケナコルト"
  ),
  
  # ----------------------------------------------------------
  # 5. Biologics (immunosuppressive / immunomodulatory)
  # ----------------------------------------------------------
  "Biologics" = c(
    # anti-TNF
    "INFLIXIMAB", "REMICADE",
    "ADALIMUMAB", "HUMIRA",
    "ETANERCEPT", "ENBREL",
    "GOLIMUMAB", "SIMPONI",
    "CERTOLIZUMAB", "CIMZIA",
    
    # IL-6 pathway
    "TOCILIZUMAB", "ACTEMRA",
    "SARILUMAB", "KEVZARA",
    
    # CTLA4-Ig / B cell / complement
    "ABATACEPT", "ORENCIA",
    "RITUXIMAB", "RITUXAN",
    "OFATUMUMAB",
    "BELIMUMAB",
    "ECULIZUMAB",
    
    # IL-12/23, IL-17
    "USTEKINUMAB", "STELARA",
    "SECUKINUMAB", "COSENTYX",
    
    # gut-selective
    "VEDOLIZUMAB", "ENTYVIO",
    
    # Japanese (validated in JADER v5.1)
    "インフリキシマブ",
    "アダリムマブ",
    "エタネルセプト",
    "ゴリムマブ",
    "セルトリズマブ",
    "トシリズマブ",
    "サリルマブ",
    "アバタセプト",
    "リツキシマブ",
    "オファツムマブ",
    "エクリズマブ",
    "ウステキヌマブ",
    "セクキヌマブ",
    "ベドリズマブ",
    "バシリキシマブ"
  )
)

# ----------------------------------------------------------
# End of file
# ----------------------------------------------------------
