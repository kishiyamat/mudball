# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

listBackwardEliminatedModels = function(model,i=0,beeping=F,ps=list()){
  require(lme4)
  if(beeping){
    require(beepr)
    beep()
  }
  # 引数には１１までの数字が入る。
  message("###########################################")
  print(i)
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
  print(min_list)
  # > print(min_list)
  # [1] "ItemNo"            "npi:aff"           "0.105571870337726"

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

  message(paste("The old formula is: ",lme_formula))
  print(variances)
  message(paste("The old formula is: ",lme_formula))
  message(sprintf('removed %s in %s',min_list[2], min_list[1]))
  message(paste("The new formula is: ",new_line))
  i = i + 1
  print(min_list)

  if(is.null(min_list[2][[1]])){
    if(beeping){beep(5)}
    return(ps)
  }

  old_model_name = deparse(substitute(model))
  new_model_name = paste(old_model_name, as.character(i),sep = "_")
  str_formula = sprintf('%s = lmer(%s, data=%s)', new_model_name, new_line, lme_data)
  print(str_formula)
  # strの実行
  eval(parse(text=str_formula))
  # これをsprintf(retrun(backward(),%s),new_model_name)みたいにすれば良いのでは？
  # return で終わるのは確かだけど、そこでさらに呼べばいい。
  appender = sprintf('ps = append(ps, %s)', new_model_name)
  eval(parse(text=appender))
  print(appender)
  print(ps)
  recall = sprintf('listBackwardEliminatedModels(%s, i, beeping, ps=ps)', new_model_name)
  eval(parse(text=recall))
}
