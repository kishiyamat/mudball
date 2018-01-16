# step

:snake: 動作まえに `save()` などでモデルを保存してください。

`step()` は `lmer` や `glmer` で作られたモデルを項に取った後、
Backward Elimination で作成されたモデルをリストとして返します。

## Prerequisite

`lme4`と`devtools`パッケージが必要です。

```R
install.packages("lme4")
install.packages("devtools")
```

## how to use

`devtool` に含まれる `install_github` 関数を使い、Github のリポジトリからインストールします。

:snake: WindowsとMacでの動作は未確認です。
:snake: Windowsの場合、少なくとも自分は管理者権限で行った分は成功しました。

```R
require(lme4)
require(devtools)
install_github("kisyaman/umbel")
require(step)

# 検証したいモデルの最大モデルを組む
model = model
#
models = step(model,beeping=T)

```

## 質問

[issue](https://github.com/kisyaman/step/issues)をご利用ください。

## License
MIT
