# The MIT License (MIT)
#
# Copyright (c) 2018 kishiyama.
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.

# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

# res = anova(models[[7]],models[[6]])
# pp = res$Pr[!is.na(res$Pr)]
# if(length(pp)!=1){
#   warning("何かおかしい")
# }
# if(pp<0.01){
#   print("有意差")
# }

step = function(model,i=0,beeping=F,ps=list(),p=0.05){
  # パッケージの読み込み
  require(lme4)
  if(beeping){
    require(beepr)
    beep()
  }
  # 終了条件のフラグ
  is_significant = FALSE
  is_done = FALSE

  message("############is_significant_start###################")
  # 最初ならばpsにモデルを追加する
  # 違うならば、psが2つ以上なのでanovaにかけてis_significantを更新する
  if(length(ps)==0){
    old_model_name = deparse(substitute(model))
    appender = sprintf('ps = append(ps, %s)', old_model_name)
    eval(parse(text=appender))
  }else{
    res = anova(rev(ps)[[1]],rev(ps)[[2]])
    pp = res$Pr[!is.na(res$Pr)]
    if(pp<p){
      is_significant = TRUE
      print("以下の２つのモデルに有意差がありました。")
      print(rev(ps)[[1]])
      print(rev(ps)[[2]])
      if(beeping){beep(5)}
    }
  }
  message("################is_significant_done###############")
  message("現在のプロセス番号：")
  message(i)
  # 最小のモデルまで行きました。
  model_summary = summary(model)
  rand_factors = as.list(rownames(as.data.frame(model_summary$ngrps)))
  variances = model_summary$varcor

  # "stddev_Hoge" に variances$Hoge の stddev を保存する * lenght(rand_factors)
  stds = list()
  for(rand_factor in rand_factors){
    # std_Hoge
    rand_factor_std = sprintf('std_%s', rand_factor)
    # attr(variances$Hoge,"stddev")
    get_rand_factor_std = sprintf('attr(variances$%s,"stddev")', rand_factor)
    # stddev_Hoge <= attr(variances$Hoge,"stddev")
    assign(rand_factor_std,as.data.frame(eval(parse(text=get_rand_factor_std))))
    stds = append(stds, rand_factor_std)
  }
  # > stds
  # [[1]]
  # [1] "std_ParticipantName"
  # [[2]]
  # [1] "std_ItemNo"

  # stddev_Hoge の中から最も小さい std と、
  # 該当する Hoge, と切片名を取得し min_list に格納する
  # 適当に大きい数字をとりあえず置く
  min_std = 10000
  min_list = list()
  for(rand_factor in rand_factors){
    # 行数が1なら、Interceptしかない、ということ。走査範囲外。
    if (nrow(get(sprintf('std_%s', rand_factor)))==1) next
    # Intercept以外([-1,])で最小値を求める
    tmp_min = min(get(sprintf('std_%s', rand_factor)) [-1,])
    if(tmp_min<=min_std){
      min_std=tmp_min
      list_of_vars = get(sprintf('std_%s', rand_factor))[,1]
      # 最も小さい値のインデックスを取得し（必ずIntercept以外）、
      min_index = which(list_of_vars == min_std)[1]
      # そのインデックスを持つ変数の名前を取得し、
      min_name = rownames(get(sprintf('std_%s', rand_factor))) [min_index]
      # リストにランダム要素、変数名、stdの値を入れる
      min_list = c(rand_factor,min_name,min_std)
    }
  }
  # print(min_list)
  # [1] "ItemNo"            "npi:aff"           "0.105571870337726"

  is_done = is.null(min_list[2][[1]])

  if(is_done || is_significant){
    if(beeping){beep(5)}
    # もし有意差がでたなら、古いモデルを
    # 出ていないなら、最もシンプルなモデル（一番新しく追加されたもの）を
    if(is_significant){
      return(rev(ps)[[2]])
    }else{
      return(rev(ps)[[1]])
    }
  }

  rownames(get(sprintf('std_%s', rand_factor)))
  rand_intercepts = list()
  for(rand_factor in rand_factors){
    # min があるファクター名ならば
    if(rand_factor==min_list[1]){
      x = rownames(get(sprintf('std_%s', rand_factor)))[-1]
      y = x[x!=min_list[2]]
      if(length(y)==0){
        rand_intercept = sprintf('(1 |%s)',rand_factor)
      }else{
        z = paste(y, collapse=" + ")
        rand_intercept = sprintf('(1 + %s|%s)',z ,rand_factor)
      }
      rand_intercepts = append(rand_intercepts, rand_intercept)
    }else{
      x = rownames(get(sprintf('std_%s', rand_factor)))[-1]
      y = x
      if(length(y)==0){
        rand_intercept = sprintf('(1 |%s)',rand_factor)
      }else{
        z = paste(y, collapse=" + ")
        rand_intercept = sprintf('(1 + %s|%s)',z ,rand_factor)
      }
      rand_intercepts = append(rand_intercepts, rand_intercept)
    }
  }

  # Rondom Factor and Intercept
  rs = paste(rand_intercepts, collapse=" + ")

  formula = as.character(model_summary$call)
  lme_function = formula[1]
  lme_formula = formula[2]
  lme_data = formula[3]
  dependent_independent = strsplit(lme_formula , "\\(")[[1]][1]
  new_line = paste(dependent_independent,rs)
  message("########### 新しい式づくり start ##################")
  message(paste("The old formula is: ",lme_formula))
  message("下がstdです。")
  print(variances)
  message(sprintf('最もstdが小さいのは %s の中の %s なので、これを削ります', min_list[1], min_list[2]))
  message("その結果、以下の式が得られます")
  message(paste("The new formula is: ",new_line))
  message("############ 新しい式づくり end ################")

  # モデルの名前を更新します
  i = i + 1

  message("############ 新しいモデル名にモデルを格納し、psも更新します ################")
  old_model_name = deparse(substitute(model))
  new_model_name = paste(old_model_name, as.character(i),sep = "_")
  str_formula = sprintf('%s = lmer(%s, data=%s)', new_model_name, new_line, lme_data)
  eval(parse(text=str_formula))
  appender = sprintf('ps = append(ps, %s)', new_model_name)
  eval(parse(text=appender))
  recall = sprintf('step(%s, i, beeping, ps=ps, p=p)', new_model_name)
  eval(parse(text=recall))
}

# models を anova してくれます。
anovaModels = function(models){
  models_var_name = deparse(substitute(models))
  in_anova = eval(parse(text=
  sprintf('paste("%s[[",1:length(%s),"]]",sep="",collapse=",")',
      models_var_name,
      models_var_name)))
  anova_result = eval(parse(text=paste("anova(",in_anova,")")))
  return(anova_result)
}
