Drug-class Dictionary v5.1
Construction, QC, and Implications for JADER External Validation
Project context

本研究围绕 常用免疫抑制剂相关药物性肝损伤（DILI），在 FAERS 数据库中完成模型构建后，使用 JADER 数据进行外部验证（external validation）。
在 JADER 验证过程中，我们发现模型性能异常偏低，经系统排查后确认：问题不在模型本身，而在于 JADER 中免疫抑制剂药物暴露的构建存在严重系统性缺失。

因此，本阶段的核心目标是：

重建并冻结一个在 FAERS 与 JADER 中均可稳定使用的免疫抑制剂 drug-class 字典（v5.1），以修复外部验证阶段的暴露误分类偏倚。

1. 原始问题的定位（Root cause analysis）
1.1 表面现象

JADER external validation 中：

AUROC 偏低

校准曲线系统性偏离

与 FAERS 内部表现不一致

1.2 初步排查（已证伪的假设）

❌ FAERS 模型过拟合

❌ 特征工程或 LASSO 选择不稳

❌ DILI 结局定义错误（SOC 级别定义保持一致）

1.3 真正的问题

在对 JADER MASTER 文件进行系统 QC 后确认：

JADER 中 >99% 的 DRUGNAME_EN 为非 ASCII（日文）

旧版免疫抑制剂暴露变量（EXPOSED_CNI / _BIO / _CS / ...）：

基于极早期、不完整的英文或有限关键词构建

严重漏检 Biologics 与 Corticosteroids

结果：

大量真实“已暴露病例”被错误标记为未暴露

→ exposure misclassification bias

→ 外部验证性能被系统性拉低

2. 设计原则（Design principles for v5.1）

drug-class v5.1 的构建遵循以下原则：

研究级完整，而非百科全书式完整

目标：覆盖真实高频免疫抑制剂暴露

不追求理论上“所有可能药物”

FAERS / JADER 双数据库通用

同一字典可直接用于两个系统

不引入数据库特异性 hard-code

多语言鲁棒性

同时支持：

英文通用名 / 商品名

日文通用名 / 商品名

避免依赖外部日文标准药品辞典（不可复现）

QC-driven 迭代

每一次扩展均以：

unmatched 高频药名

实际 gain（覆盖病例数）
为依据

3. 免疫抑制剂分类框架（Fixed scope）

v5.1 固定研究范围为 5 类免疫抑制剂：

Calcineurin inhibitors (CNI)

mTOR inhibitors

Antiproliferative agents (APA)

Systemic corticosteroids

Biologics (immunosuppressive / immunomodulatory)

⚠️ 明确排除：

传统 DMARD（如 MTX、Leflunomide）

抗肿瘤单抗（除非明确用于免疫抑制场景）

4. v5.1 构建流程概述
Step 1. JADER 药物字段 QC

评估字段可用性：

DRUGNAME_EN

DRUG_MOLECULE

DRUG_INN_EN

结果：

可稳定使用的核心字段：DRUGNAME_EN

非 ASCII 比例 >99%

Step 2. 初始关键词字典（v5）

基于：

FAERS 既有英文关键词

少量常见日文免疫抑制剂名

发现问题：

覆盖率极低（<15% records）

Step 3. unmatched 非 ASCII 药名 QC

对 JADER PSSS 记录中：

未被命中的高频日文药名进行排序

人工 + 规则识别：

明确属于 5 类免疫抑制剂的药物

排除噪声（剂型、给药方式、非免疫抑制剂）

Step 4. v5.1 定向扩展（Biologics 为核心）

重点补齐：

日文 biologics（如 インフリキシマブ、アダリムマブ 等）

系统性糖皮质激素常见日文商品名

每一次补充均评估：

新增覆盖病例数（gain）

是否引入明显非目标药物

Step 5. 冻结 v5.1

当 unmatched top-N 中：

不再出现明确免疫抑制剂

停止扩展，冻结字典

5. v5.1 的核心成果
5.1 字典层面

一个统一的 R 对象：

drug_classes


每一类同时包含：

英文通用名 / 商品名

日文通用名 / 商品名

可直接 source() 调用

5.2 QC 结果（定性）

相比旧版 EXPOSED_*：

Biologics：大量新增暴露病例

Corticosteroids：显著修复漏检

v5.1 暴露定义：

更接近真实 JADER 用药结构

明显减少假阴性暴露

5.3 方法学意义

证明：

JADER external validation 的主要问题
不是模型泛化失败
而是暴露误分类

v5.1 为：

重跑 external validation

合理解释前后性能差异
提供了方法学基础

6. 对后续分析的直接影响

旧 JADER external validation 结果不应直接使用

基于存在缺陷的暴露定义

必须使用 v5.1 重建 JADER case-level 暴露

在不改变 FAERS 冻结模型的前提下

性能改善的解释路径清晰

若 AUROC / calibration 改善：

属于暴露修复后的真实表现

若仍有限：

才能讨论数据库体系差异或模型边界