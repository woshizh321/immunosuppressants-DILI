常用免疫抑制剂相关药物性肝损伤（DILI）
FAERS → 建模 → JADER 验证 → Regimen & Molecule 异质性分析
研究设计与分析进展总结（Engineering + Methods）
一、研究背景与核心问题

研究主题
在真实世界联合用药（regimen）背景下，系统评估常用免疫抑制剂相关**药物性肝损伤（Drug-Induced Liver Injury, DILI）**的风险结构，并进一步解析：

不同**免疫抑制剂类别（drug class）**之间的风险差异

在联合用药情境中，不同**治疗方案（regimen）**的风险分层

同一药物类别内部，不同分子（molecule）是否存在显著风险异质性

研究定位（非常重要）

本研究不是“个体层面的预测模型”，
而是药物警戒导向的风险结构解析（risk architecture analysis）。

二、数据来源与总体框架
数据集

FAERS（主分析 + 建模）

JADER（外部验证）

免疫抑制剂分组（锁定，不再更改）

Calcineurin inhibitors (CNI)

mTOR inhibitors

Antiproliferative agents (APA)

Corticosteroids

Biologics

药物字典路径（工程锁定）：

99_shared/dict/immunosuppressants_drug_classes.R

三、DILI 结局定义（已冻结）

主要结局（用于所有建模）

MedDRA SOC = “Hepatobiliary disorders”

PT 层级

仅用于信号浏览与 Supplementary

❌ 不作为 DILI 建模结局（避免概念漂移）

与既往 DIKI 项目严格区分

四、分析流程总览（Step-by-Step）
Step 1｜信号检测（FAERS）

目录：01_signal_detection

层级：SOC → HLGT → HLT（PT 仅补充）

方法：

ROR

PRR

χ²

按 5 类免疫抑制剂分别计算（非合并）

输出：

各层级信号表

QC 文件（病例数、暴露率）

📌 目的：

建立 DILI 风险存在性的“药物警戒证据基础”

Step 2｜Model_F：LASSO（FAERS）

目录：02_model_F_LASSO

输入：case-level 数据

变量：

sex_bin（Female / Male / Other）

age_bin（<18 / 18–39 / 40–64 / 65+）

5 类免疫抑制剂（drug-level）

目的：

筛选稳定进入模型的风险因素

结果：

5 类免疫抑制剂均被保留

📌 定位：

稳健变量筛选，而非预测

Step 3｜Model_P：LASSO（保守版本）

目录：03_model_P_LASSO

使用 λ.1se

与 Model_F 方向一致

作为后续 Logistic 与外部验证的“锁定特征空间”

Step 4｜Model_E：Logistic 回归（可解释模型）

目录：04_model_E_logistic

目的：

给出 OR / 95% CI

增强可解释性

输出：

各风险因素的 OR、CI

结果：

与 LASSO 模型高度一致（方向稳定）

Step 5｜模型比较与稳健性

目录：05_model_comparison

比较：

Model_F vs Model_P vs Model_E

结论：

不同模型间风险排序与方向高度一致

说明风险结构稳定

Step 6｜外部验证（JADER）

目录：06_external_validation_JADER

使用与 FAERS 完全一致的：

变量定义

drug-level 特征空间

结果：

AUROC ≈ 0.52（≈ 随机）

但 风险方向一致

解释：

本研究目标为 风险解析，而非预测性能最大化

📌 结论：

外部数据支持风险结构的可迁移性

Step 7｜Regimen-aware 风险画像

目录：07_risk_portrait

背景假设：

免疫抑制剂在真实临床中联合使用

方法：

基于 Model_E 的 β 系数

构建 sex × age × regimen 的组合风险

输出：

高 / 中 / 低风险联合方案

价值：

提供“临床可读”的风险分层视角

Step 8｜Molecule-level 异质性分析（DILI 专属）

目录：08_molecule_heterogeneity_DILI

关键设计原则

使用 FAERS MASTER parquet

DuckDB 硬盘读取（不使用 arrow，避免内存问题）

DILI 仍然基于 SOC 层级

regimen-aware 调整（sex / age / 其他 drug class）

方法学修正（关键）

❌ 避免互斥 dummy（如 tacrolimus + cyclosporine 同时入模）

✅ within-class 使用 factor(molecule)

核心结果（CNI）

Tacrolimus vs Cyclosporine

OR = 1.70

95% CI = 1.40 – 2.06

📌 结论：

在相同联合用药背景下，同为 CNI，Tacrolimus 的 DILI 报告风险显著高于 Cyclosporine，提示显著的分子层级风险异质性。

五、工程化目录结构（当前锁定）
immunosuppressants_DILI/
├── 00_project
├── 01_signal_detection
├── 02_model_F_LASSO
├── 03_model_P_LASSO
├── 04_model_E_logistic
├── 05_model_comparison
├── 06_external_validation_JADER
├── 07_risk_portrait
├── 08_molecule_heterogeneity_DILI
├── 98_legacy_logistic
└── 99_shared
    ├── dict
    ├── functions
    ├── QC
    ├── tmp_duckdb
    └── utils

六、研究状态总结

✅ 所有分析工作已完成

✅ 风险定义、模型、验证与分子异质性形成闭环

❌ 不再需要新增分析（如 SHAP、深度学习、PT 扩展）

Step 8（补充）｜Molecule-level Risk Portrait 与起病时间（Figure 6B–6C）

在完成分子层级风险异质性识别（within-class 对比）后，我们进一步从人群分层与时间维度对代表性分子进行精细刻画，以回答两个关键问题：

1）在相同的总体风险结构下，不同人群（sex × age）中，单个分子的组合风险如何变化？
2）这些分子的 DILI 风险是否表现出不同的起病时间谱（time-to-onset, TTO）？

为此，我们在 FAERS 主数据集中开展了 Figure 6B（risk portrait）与 Figure 6C（TTO）分析。

Figure 6B｜Molecule-level risk portrait（sex × age × molecule）

分析对象（锁定）
基于前述分子层级异质性结果，选取 4 个具有代表性的免疫抑制剂分子：

Tacrolimus（CNI）

Basiliximab（Biologics）

Azathioprine（APA）

Everolimus（mTOR inhibitors）

方法概述

数据来源：FAERS MASTER parquet（case-level）

DILI 定义：MedDRA SOC = Hepatobiliary disorders（与全文一致）

分层变量：sex（Female / Male / Other） × age（<18 / 18–39 / 40–64 / ≥65）

模型参数：来自 Model_F（LASSO, λ = 1se）的稳定 β 系数

计算方式：

𝑂
𝑅
𝑐
𝑜
𝑚
𝑏
𝑜
=
exp
⁡
(
∑
𝛽
𝑖
𝑋
𝑖
)
OR
combo
	​

=exp(∑β
i
	​

X
i
	​

)


其中 
𝑋
𝑖
X
i
	​

 表示分子暴露、性别及年龄分层指示变量。

可视化设计（Figure 6B）

图形形式：二维 heatmap（risk portrait）

x 轴：年龄分组

y 轴：性别

分面（facet）：分子（molecule）

颜色映射：OR_combo（以 OR = 1 为中点）

核心信息
Figure 6B 揭示了显著的 molecule-specific 且 population-dependent 风险异质性：

Tacrolimus 与 Azathioprine 在男性及高龄人群中呈现更高的组合风险；

Basiliximab 的风险结构对年龄依赖性较弱，呈现相对“早期型”风险特征；

Everolimus 的风险分布介于上述两类之间。

Figure 6C｜Time-to-onset（TTO）异质性分析

在风险画像（Figure 6B）的基础上，我们进一步引入起病时间维度，用于补充风险的时间学特征。

TTO 定义与数据来源

使用 FAERS MASTER 中已计算的 TTO 字段：tto_onset_days_psss

暴露口径：Primary Suspect / Secondary Suspect（PS/SS），与前述风险分析保持一致

仅纳入 DILI = 1 的病例用于起病时间分析

数据处理要点

对每一病例–分子组合，取最早的正向 TTO（最短起病时间）；

在分子内部进行 1st–99th percentile 过滤，以减少极端值对分布的影响；

起病模式分层：

Early：≤30 天

Intermediate：31–180 天

Late：>180 天

可视化设计（Figure 6C）

主图：log 尺度箱线图（展示 TTO 的中位数与 IQR）

辅助信息：Early / Intermediate / Late 起病比例

主要发现
不同分子表现出截然不同的起病时间谱：

Basiliximab：以早期起病为主（median ≈ 3 周），提示诱导期相关风险；

Everolimus：以中期起病为主（median ≈ 2–3 月）；

Tacrolimus：呈混合型、偏晚起病分布；

Azathioprine：以显著迟发起病为特征（median > 300 天）。

方法学与整体意义

Figure 6B 与 Figure 6C 共同构成了分子层级风险异质性的双维刻画框架：

Figure 6B 回答“谁在什么人群中风险更高”；

Figure 6C 回答“风险通常在何时出现”。

这一设计将前述模型结果（Figure 4–5）与分子异质性分析自然衔接，为 DILI 的药物警戒与临床风险认知提供了更具可解释性的补充证据。

下一阶段工作（在新对话中进行）：

📊 可视化（Figures）

📝 Results 与 Discussion 写作
