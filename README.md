# Mudball

:snake: 実行前にモデルを `save`関数などで保存してください。

`mudball::step.bw()` は
1. `lmer` や `glmer` で作られたモデルを項に取り、
1. Backward Elimination で作成されたモデルのリストの返します。

## Prerequisite

`step.bw`を動かすには
`lme4` と `magrittr`, `tidyverse`, `devtools` パッケージが必要です。

```R

install.packages("lme4")
install.packages("magrittr")
install.packages("tidyverse")
install.packages("devtools")
require(lme4)
require(magrittr)
require(tidyverse)
require(devtools)
```

## How to use

`devtool` に含まれる `install_github` 関数を使い、
Github のリポジトリからインストールします。

:snake: WindowsとMacでの動作は未確認です。
:snake: Windowsの場合、少なくとも自分は管理者権限で行った分は成功しました。

```R

install_github("kisiyama/mudball")
require(mudball)

# 検証したいモデルの最大モデルをstep.bwに入れる
model.list <- 
lmer(rt.word1.offset~mod.s*head.s+
    (1+mod.s*head.s|subject) +
    (1+mod.s*head.s|item),
    data=data.tidy.scaled) %>%
mudball::step.bw

# 結果を比較。
anova %>% do.call(model.list)↲
```

## 質問

[issue](https://github.com/kisiyama/mudball/issues)をご利用ください。

## License

MIT
