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
require(lme4)
require(magrittr)
require(dplyr)
step.bw <- function(model,log=TRUE){
    # rand(om effect)における最も小さいvarianceを除外したい
    varcorr.df.rand <- 
        model %>% VarCorr %>% as.data.frame %>% 
        ## interceptでもrasidualでもないvarcorr
        filter(is.na(var2))    %>% select(-var2)%>%
        filter(!(is.na(var1))) %>% filter(var1!="(Intercept)") 
    ## でもその前にrandomが0なら? それは再帰の基底部
    ## 最も単純なモデルは削れないのでそのモデルを返して再帰を終了
    model.summary <- model %>% summary
    model.call    <- model.summary %>% "$"(call)
    var.min.i   <-which.min(varcorr.df.rand$vcov)
    if(log){
        print("今のモデルは以下です。")
        print(model.call)
        print("summaryは以下です。")
        print(model.summary)
        print("以下の項を削り式を作ります。")
        var.min     <- varcorr.df.rand[var.min.i,]
        print(var.min)
    }
    if(nrow(varcorr.df.rand)==0){
        print("これ以上式を削れません")
        return(model)
    }
    ## 気を取り直して最も小さいvarianceのidを探して除外
    var.not.min <-varcorr.df.rand[1:nrow(varcorr.df.rand)!=var.min.i,]
    # 上で最も小さいvarianceが分かったので式から削る
    ## まずはmodel.callからfunction, formula, dataを抽出
    fun           <- model.call[1] %>% as.character
    data          <- model.call[3] %>% as.character
    formula.parts <- model.call[2] %>% as.character %>%
                         str_replace_all(" ","") %>%
                         strsplit(.,split="\\+\\(") %>% unlist
    fixed.part    <- formula.parts[1]
    ## random.partsはlistになります
    random.parts  <- formula.parts[1:length(formula.parts)!=1]
    ## random factor は|ではじまり)で終わる.その部分を抽出
    random.factors <- random.parts %>%
        ## まずrandom.partごとに|と)の場所の同定
        str_locate(regex("[\\|]+[\\w]*[\\)]")) %>%
        ## random.partのそれぞれの横にstartとendをbind
        cbind(random.parts) %>% as.data.frame %>%
        mutate(start    =as.numeric(as.character(start))+1,
               end      =as.numeric(as.character(end))-1,
               r.factors=str_sub(random.parts,start=start,end=end))
    ## random factors ずつ式を立てる。rfの数に行が圧縮される
    random.df.newlines <- random.factors %>%
        ## group_by(random.parts) %>%
        group_by(r.factors) %>%
        mutate(r.effects=(r.factors %>%
                            (function(x)
                                var.not.min %>% filter(grp==x) %>%
                                # var1は切片なので|の左側で+結合
                                select(var1) %>% unlist %>% 
                                paste(collapse="+"))),
               r.newlines=ifelse(nchar(r.effects)!=0,
                          sprintf("(1+%s|%s)",r.effects,r.factors),
                          sprintf("(1|%s)",r.factors)))

    random.parts.new <- paste(random.df.newlines$r.newlines, collapse="+")
    formula.new <- paste(fixed.part, random.parts.new, sep="+")
    ## 削ったので式を統合
    model.next.str <-sprintf("%s(%s,data=%s)",fun,formula.new,data)
    if(log){
        print("すると次のモデルは以下となります。")
        print(model.next.str)
    }
    model.next <- eval(parse(text=model.next.str))
    return(c(model, step.bw(model.next,log)))
}
