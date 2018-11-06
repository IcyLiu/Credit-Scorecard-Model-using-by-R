######################################################################
#the data used here is between 2017.6~2018.6 by the claim ending date#
######################################################################


######################################################################
#################### 1. 从最原始数据至转换后的数据 ###################
######################################################################

##################### 1.1 手动筛选原始数据 ######################
{		
  setwd("~/2017年1月至2018年6月数据")
  data <- read.csv("理赔项目201707-201806数据.csv", header=TRUE, sep=",", na.strings = "", blank.lines.skip=F)
  
  ###transform the data 
  names(data)
  str(data)
  head(data)
  
  #get the target according to business
  ##险种理赔拒赔原因
  levels(data$险种理赔拒赔原因) # converting the format of dependent variable
  levels(data$险种理赔拒赔原因)[which(levels(data$险种理赔拒赔原因)=="不符合定义（疾病）"
                              |levels(data$险种理赔拒赔原因)=="不实告知"
                              |levels(data$险种理赔拒赔原因)=="其它（保险欺诈）")] <- "1"
  levels(data$险种理赔拒赔原因)[which(levels(data$险种理赔拒赔原因)=="责任免除"&levels(data$是否责任免除)=="N")] <- "1"
  levels(data$险种理赔拒赔原因)[which(levels(data$险种理赔拒赔原因)!="1")] <- "0"
  
  data$险种理赔拒赔原因[is.na(data$险种理赔拒赔原因)] <- 0
  
  length(which(data$险种理赔拒赔原因==1))
  length(which(data$险种理赔拒赔原因==0))
  class(data$险种理赔拒赔原因)
  as.matrix(names(data))
  
  var_01<-c("保单ID","案件ID号","保单险种选项ID","险种理赔拒赔原因","险种责任","案件事故性质","当前案件理赔申请次数","是否首次申请理赔","代理人是否在职",
            "保单所属二级机构代码","保单所属机构代码","销售渠道","出单渠道","保单销售方式","投保人与主被保人关系","保单质押状态",
            "险种内部代码","险种投保档次","险种投保份数","保单险种来源","险种主附约别","保障年期类型","标准保障年限或年龄","险种失效原因",
            "险种是否暂停","险种投保金额","削减保额","标准体当期保费","加费方法","保证给付年限","险种给付状态","给付方式","给付结束年期类型",
            "给付结束年限或年龄","给付开始年期类型","给付开始年限或年龄","当期缴费方式","缴费年期类型","缴费年限或年龄","险种缴费年度",
            "险种缴费状态","险种实际缴费期次","保费缴付年期","职业加费方式","次标体开始加费类型","次标体加费比例","次标体加费方式",
            "业务员直属部门代码","业务员直属机构ID","业务员所属二级机构ID","业务员性别","业务员文化程度","业务员婚姻状况","业务员状态",
            "是否代理人自保件","业务员品质职级","业务员分类","业务员入司时间","二次入司标志代码","绿色通道授权状态代码",
            "客户性别代码","客户身高","客户证件类型ID","客户收入","客户婚姻状况","客户是否是一家之主","职业ID","手机号码等级代码",
            "客户等级代码","客户等级","高端客户标识2","规保客户积分","是否社保","健康险EM值","寿险EM值","是否吸烟",
            "保单寿险保额","保单意外险保额","保单人身险保额","保单重疾险保额","险种重疾保额","险种人身险保额","险种意外险保额","险种重疾险保额","险种保额",
            "自动核保结论","业务年度","是否契调","核保限制档次","险种核保决定结论","是否体检","续保决定",
            "案件类型","案件业务类型","案件业务来源","诊断结果","立案时效","是否原件或复印件",
            "住院合计费用","住院统筹支付费用","住院其他扣除","住院自费费用","住院自付费用",
            "门诊合计费用","门诊统筹支付费用","门诊其他扣除","门诊自费费用","门诊自付费用",
            "被保人医疗险年度保费总额","此保单年度保费总额","责任赔付金额","发票总金额","自费总金额","其它扣除",
            "床位费.住院","手术费.住院","西药费.住院","检查费.住院","特殊检查费.住院","护理费.住院","中成药费.住院","化验费.住院","中草药费.住院","放射费.住院","诊察费.住院","输血费.住院","材料费.住院",
            "其他.住院","西药费.门诊","检查费.门诊","手术费.门诊","中成药费.住院.1","化验费.住院.1","治疗费.门诊","中草药费.门诊","放射费.门诊","诊察费.门诊","特殊检查费.门诊","材料费.门诊","其他.门诊",
            "住院或门诊","在医天数","是否慢性病","历史理赔案件数","历史理赔总额","历史理赔险种","是否曾经被认定为欺诈")
  data_01<-data[var_01]
  head(data_01)
  str(data_01)
}

################ 1.2 衍生变量的产生（时间类） ###################
{
  ##案件理赔申请日期减案件出险日期
  ##代理人离职日期减保单生效日期
  ##案件出险日期减代理人离职时间
  ##案件理赔申请日期减代理人离职时间
  ##案件出险日期减险种生效时间
  ##案件理赔申请日期减险种生效时间
  案件出险日期<-as.Date(data$案件出险日期)
  保单生效日期<-as.Date(data$保单生效日期)
  代理人离职时间<-as.Date(data$代理人离职时间)
  summary(代理人离职时间)
  案件出险日期<-as.Date(data$案件出险日期)
  案件理赔申请日期<-as.Date(data$案件理赔申请日期)
  险种生效时间<-as.Date(data$险种生效时间)
  
  案件理赔申请日期减案件出险日期<-as.numeric(案件理赔申请日期-案件出险日期)
  代理人离职时间减保单生效日期<-as.numeric(代理人离职时间-保单生效日期)
  案件出险日期减代理人离职时间<-as.numeric(案件出险日期-代理人离职时间)
  案件理赔申请日期减代理人离职时间<-as.numeric(案件理赔申请日期-代理人离职时间)
  案件出险日期减险种生效时间<-as.numeric(案件出险日期-险种生效时间)
  案件理赔申请日期减险种生效时间<-as.numeric(案件理赔申请日期-险种生效时间)
  
  summary(案件理赔申请日期减案件出险日期)
  summary(代理人离职时间减保单生效日期)
  summary(案件出险日期减代理人离职时间)
  summary(案件理赔申请日期减代理人离职时间)
  summary(案件出险日期减险种生效时间)
  summary(案件理赔申请日期减险种生效时间)
  
  
  ##业务员年龄
  ##业务员入司年限
  ##客户年龄
  业务员生日<-as.Date(data$业务员生日)
  业务员入司时间<-as.Date(data$业务员入司时间)
  客户生日<-as.Date(data$客户生日)
  today<-Sys.Date()
  业务员年龄<-as.numeric(round(difftime(today,业务员生日,units="auto")/365,digits=0))
  业务员入司年限<-as.numeric(round(difftime(today,业务员入司时间,units="auto")/365,digits=0))
  客户年龄<-as.numeric(round(difftime(today,客户生日,units="auto")/365,digits=0))
  
  summary(业务员年龄)
  summary(业务员入司年限)
  summary(客户年龄)
  
  ##是否有工作单位名称
  ##是否有工作单位地址
  ##是否有客户联系地址
  是否有工作单位名称 <- ifelse(is.na(data$工作单位名称), 0, 1)
  是否有工作单位地址 <- ifelse(is.na(data$工作单位地址), 0, 1)
  是否有客户联系地址 <- ifelse(is.na(data$客户联系地址字段一), 0, 1)
  
  table(是否有工作单位名称)
  table(是否有工作单位地址)
  table(是否有客户联系地址)
}

############### 1.3 衍生变量的产生（one-hot type）###############
{
  data_02 <- data.frame(data_01,案件理赔申请日期减案件出险日期,代理人离职时间减保单生效日期,
                        案件出险日期减代理人离职时间,案件理赔申请日期减代理人离职时间,案件出险日期减险种生效时间,
                        案件理赔申请日期减险种生效时间,业务员年龄,业务员入司年限,客户年龄,是否有工作单位名称,
                        是否有工作单位地址,是否有客户联系地址)
  data_02 <- subset(data_02,select=-c(业务员入司时间))
  #write.csv(data_02, file = "20180806.csv")
  #setwd("~/2017年1月至2018年6月数据")
  #data_02 <- read.csv("20180806.csv", header=TRUE, sep=",", na.strings = "", blank.lines.skip=F)
  
  # Categories to Dummy - When categorical variable needs to separate
  ## 申请类型字段
  setwd("~/")
  testing <- read.csv("申请类型-testing.csv")
  library(reshape2)
  head(testing)
  split <- melt(testing, id.vars=c("ID", "保单ID", "案件ID号", "保单险种选项ID"))
  split <- split[with(split, order(ID, 保单ID, 案件ID号, 保单险种选项ID)), ]
  split <- split[,-5]
  split <- split[!(is.na(split$value) | split$value==""), ]
  split$value <- as.factor(split$value)
  levels(split$value)
  B <- model.matrix(ID + 案件ID号 + 保单ID + 保单险种选项ID ~ ID + 案件ID号 + 保单ID + 保单险种选项ID + value, split)
  B <- as.data.frame(B)
  B$value残疾保险金 <- 0 # Debug
  for (i in 1:nrow(B)){
    if (B[i,"value豁免保险金"]=="0"&B[i,"value门诊费用"]=="0"&B[i,"value烧烫伤保险金"]=="0"&B[i,"value身故保险金"]=="0"
        &B[i,"value手术费用"]=="0"&B[i,"value特种疾病津贴"]=="0"&B[i,"value重大疾病保险金"]=="0"&B[i,"value重症监护津贴"]=="0"
        &B[i,"value住院费用"]=="0"&B[i,"value住院津贴"]=="0")
    {B[i,"value残疾保险金"] <- 1}
  }
  B1 <- aggregate(x=B[c("value残疾保险金","value豁免保险金","value门诊费用","value烧烫伤保险金","value身故保险金","value手术费用",
                        "value特种疾病津贴","value重大疾病保险金","value重症监护津贴","value住院费用",
                        "value住院津贴")], by=list(B$ID, B$保单ID, B$案件ID号, B$保单险种选项ID), sum, na.rm = TRUE)
  colnames(B1)[1] <- "ID"
  colnames(B1)[2] <- "保单ID"
  colnames(B1)[3] <- "案件ID号"
  colnames(B1)[4] <- "保单险种选项ID"
  M <- merge(testing, B1, by.x=c("ID", "保单ID", "案件ID号", "保单险种选项ID"), by.y=c("ID", "保单ID", "案件ID号", "保单险种选项ID"))
  M <- M[with(M, order(ID)), ]
  M <- M[,-c(5:13)]
  # write.csv(M, file = "申请类型字段.csv")
  data_02$ID <- 1:nrow(data_02)
  data_03 <- merge(data_02, M, by=c("ID", "保单ID", "案件ID号", "保单险种选项ID"), all = TRUE)
  data_03 <- data_03[with(data_03, order(ID)), ]
  
  ## 案件事故结果名称字段
  testing <- read.csv("案件事故结果名称-testing.csv")
  head(testing)
  split <- melt(testing, id.vars=c("ID", "保单ID", "案件ID号", "保单险种选项ID"))
  split <- split[with(split, order(ID, 保单ID, 案件ID号, 保单险种选项ID)), ]
  split <- split[,-5]
  split <- split[!(is.na(split$value) | split$value==""), ]
  split$value <- as.factor(split$value)
  levels(split$value)
  B <- model.matrix(ID + 案件ID号 + 保单ID + 保单险种选项ID ~ ID + 案件ID号 + 保单ID + 保单险种选项ID + value, split)
  B <- as.data.frame(B)
  B$value残疾 <- 0 # Debug
  for (i in 1:nrow(B)){
    if (B[i,"value豁免"]=="0"&B[i,"value疾病末期"]=="0"&B[i,"value门诊"]=="0"&B[i,"value其它"]=="0"
        &B[i,"value烧烫伤"]=="0"&B[i,"value身故"]=="0"&B[i,"value手术"]=="0"&B[i,"value特种疾病津贴"]=="0"
        &B[i,"value医疗疾病"]=="0"&B[i,"value重疾"]=="0"&B[i,"value重症监护"]=="0"&B[i,"value住院"]=="0"
        &B[i,"value残疾"]=="0")
    {B[i,"value残疾"] <- 1}
  }
  B1 <- aggregate(x=B[c("value豁免","value疾病末期","value门诊","value其它","value烧烫伤","value身故","value手术",
                        "value特种疾病津贴","value医疗疾病","value重疾","value重症监护","value住院","value残疾")], 
                  by=list(B$ID, B$保单ID, B$案件ID号, B$保单险种选项ID), sum, na.rm = TRUE)
  colnames(B1)[1] <- "ID"
  colnames(B1)[2] <- "保单ID"
  colnames(B1)[3] <- "案件ID号"
  colnames(B1)[4] <- "保单险种选项ID"
  M <- merge(testing, B1, by.x=c("ID", "保单ID", "案件ID号", "保单险种选项ID"), by.y=c("ID", "保单ID", "案件ID号", "保单险种选项ID"))
  M <- M[with(M, order(ID)), ]
  M <- M[,-c(5:10)]
  # write.csv(M, file = "案件事故结果名称字段.csv")
  data_03 <- merge(data_03, M, by=c("ID", "保单ID", "案件ID号", "保单险种选项ID"), all = TRUE)
  data_03 <- data_03[with(data_03, order(ID)), ]
  
  ## 特定保全项字段
  testing <- read.csv("特定保全项-testing.csv")
  head(testing)
  split <- melt(testing, id.vars=c("ID", "保单ID", "案件ID号", "保单险种选项ID"))
  split <- split[with(split, order(ID, 保单ID, 案件ID号, 保单险种选项ID)), ]
  split <- split[,-5]
  split <- split[!(is.na(split$value) | split$value==""), ]
  split$value <- as.factor(split$value)
  split <- unique(split[,1:5])
  levels(split$value)
  B <- model.matrix(ID + 案件ID号 + 保单ID + 保单险种选项ID ~ ID + 案件ID号 + 保单ID + 保单险种选项ID + value, split)
  B <- as.data.frame(B)
  B$value101个单投保人变更 <- 0 # Debug
  for (i in 1:nrow(B)){
    if (B[i,"value102客户基本资料变更"]=="0"&B[i,"value103受益人变更"]=="0"&B[i,"value108个单新增附约"]=="0"&B[i,"value117职业变更"]=="0"
        &B[i,"value125个单补充告知"]=="0"&B[i,"value161个单生日性别更正"]=="0"&B[i,"value388停效改有效"]=="0"&B[i,"value401个单普通复效"]=="0"
        &B[i,"value415免息复效"]=="0")
    {B[i,"value101个单投保人变更"] <- 1}
  }
  B1 <- aggregate(x=B[c("value101个单投保人变更","value102客户基本资料变更","value103受益人变更","value108个单新增附约","value117职业变更","value125个单补充告知",
                        "value161个单生日性别更正","value388停效改有效","value401个单普通复效","value415免息复效")], 
                  by=list(B$ID, B$保单ID, B$案件ID号, B$保单险种选项ID), sum, na.rm = TRUE)
  colnames(B1)[1] <- "ID"
  colnames(B1)[2] <- "保单ID"
  colnames(B1)[3] <- "案件ID号"
  colnames(B1)[4] <- "保单险种选项ID"
  M <- merge(testing, B1, by.x=c("ID", "保单ID", "案件ID号", "保单险种选项ID"), by.y=c("ID", "保单ID", "案件ID号", "保单险种选项ID"))
  M <- M[with(M, order(ID)), ]
  M <- M[,-c(5:54)]
  # write.csv(M, file = "特定保全项字段.csv")
  data_03 <- merge(data_03, M, by=c("ID", "保单ID", "案件ID号", "保单险种选项ID"), all = TRUE)
  data_03 <- data_03[with(data_03, order(ID)), ]
  
  ## 险种保障责任代码名称字段
  testing <- read.csv("险种保障责任代码名称-testing.csv")
  head(testing)
  split <- melt(testing, id.vars=c("ID", "保单ID", "案件ID号", "保单险种选项ID"))
  split <- split[with(split, order(ID, 保单ID, 案件ID号, 保单险种选项ID)), ]
  split <- split[,-5]
  split <- split[!(is.na(split$value) | split$value==""), ]
  split$value <- as.factor(split$value)
  levels(split$value)
  B <- model.matrix(ID + 案件ID号 + 保单ID + 保单险种选项ID ~ ID + 案件ID号 + 保单ID + 保单险种选项ID + value, split)
  B <- as.data.frame(B)
  B1 <- aggregate(x=B[c("value101意外身故","value103疾病身故","value104航空意外","value105交通意外","value106疾病末期",
                        "value107通用身故","value108年金保证领取期内身故","value113双倍补贴","value119信用卡额外补偿",
                        "value120飞机意外身故","value121车船意外身故","value122赠送保额","value123身故特别给付金",
                        "value124购买投资单位前身故","value125购买投资单位后身故","value126购买投资单位前意外身故",
                        "value127购买投资单位后意外身故","value128公共交通意外身故","value130公共交通意外身故",
                        "value131私家车意外身故","value132客运机动车意外身故","value133客运列车轮船意外身故",
                        "value137轨道交通意外身故","value138航空意外身故","value139其他意外身故","value140节假日意外身故",
                        "value141高速列车意外身故","value142公共交通意外身故","value143重大自然灾害意外身故",
                        "value144法定节假日意外身故","value146投保人意外身故或全残豁免","value147水陆公共交通意外身故",
                        "value148共享单车交通意外身故","value149网约车交通意外身故","value201意外伤残","value206意外烧伤",
                        "value210通用全残","value217通用失能","value218交通意外全残","value219公共交通意外高残",
                        "value220身体高度残疾","value221私家车意外残疾","value224航空意外残疾",
                        "value226私家车及公共交通意外伤残","value301生存保险金","value302养老金给付","value303满期金",
                        "value305祝寿金","value315特别保险金","value316祝寿金","value317生存金-保费","value401重大疾病",
                        "value403重大疾病康复金/关爱金","value404原位癌","value407儿童特定疾病","value409乳房切除手术",
                        "value410癌症","value412骨质疏松症所致骨折","value413特种疾病津贴","value415意外面部整形",
                        "value423生命关爱保险金","value424特种重大疾病","value425高发重大疾病额外给付保险金",
                        "value428恶性肿瘤","value429高费用癌症","value430意外骨折医疗保险金","value431轻症保险金",
                        "value433特定癌症保险金","value434第一类重大疾病保险金","value435第二类重大疾病保险金",
                        "value436第三类重大疾病保险金","value437恶性肿瘤住院医疗费用","value438恶性肿瘤特定门诊医疗费用",
                        "value439恶性肿瘤门诊手术医疗费用","value450恶性肿瘤住院津贴保险金","value501住院津贴",
                        "value502意外住院津贴","value503癌症住院津贴","value506重病监护给付","value507重大疾病住院津贴",
                        "value520特定疾病","value521癌症疗养","value522重症津贴保险金","value523意外骨折住院津贴保险金",
                        "value601住院医疗费用","value602意外医疗费用","value603手术费用","value606住院床位费",
                        "value611住院其它医疗费","value614重大疾病住院医疗费用保险金","value626肾透析及癌症补贴",
                        "value642门（急）诊医疗费","value666医疗费用-住院前后门诊","value667医疗费用-特定门诊",
                        "value668医疗费用-慢性病门诊","value670门诊手术医疗费用","value672医疗费用-特定门诊手术",
                        "value673医疗费用-特定疾病住院","value801豁免保费","value802少儿险豁免保费","value803身故豁免",
                        "value804全残豁免","value805重大疾病豁免","value806父母身故豁免","value807父母全残豁免",
                        "value809投保人意外身故豁免","value810投保人意外全残豁免","value811特种疾病豁免","value902红利保额",
                        "value916全面保障","value920航空意外伤害","value921其他意外伤害","value923高速列车意外伤害")], 
                  by=list(B$ID, B$保单ID, B$案件ID号, B$保单险种选项ID), sum, na.rm = TRUE)
  colnames(B1)[1] <- "ID"
  colnames(B1)[2] <- "保单ID"
  colnames(B1)[3] <- "案件ID号"
  colnames(B1)[4] <- "保单险种选项ID"
  M <- merge(testing, B1, by.x=c("ID", "保单ID", "案件ID号", "保单险种选项ID"), by.y=c("ID", "保单ID", "案件ID号", "保单险种选项ID"))
  M <- M[with(M, order(ID)), ]
  M <- M[,-c(5:11)]
  # write.csv(M, file = "险种保障责任代码名称字段.csv")
  # M <- read.csv("险种保障责任代码名称字段.csv", header=TRUE, sep=",", na.strings = "", blank.lines.skip=F)
  data <- merge(data_03, M, by=c("ID", "保单ID", "案件ID号", "保单险种选项ID"), all = TRUE)
  data <- data[with(data, order(ID)), ]
  data <- subset(data,select=-c(X))
  write.csv(data, file = "finaldata.csv")
}

###################### 1.4 数据类型的转变 #######################
library(scorecard)

step1_finalData <- read.csv("D:/R/ORIGINAL_DATA/finaldata201707-201806/finaldata201707-201806.csv", header=TRUE, sep=",", na.strings = "", blank.lines.skip=F)
nrow(step1_finalData)
names(step1_finalData)
summary(step1_finalData$险种理赔拒赔原因)
data<-step1_finalData[,5:193]
names(data)
head(data)

#Change chass
{
  data$险种理赔拒赔原因<-as.numeric(data$险种理赔拒赔原因);summary(data$险种理赔拒赔原因)
  data$保单所属二级机构代码<-as.character(data$保单所属二级机构代码);summary(data$保单所属二级机构代码)
  data$险种内部代码<-as.character(data$险种内部代码);summary(data$险种内部代码)
  data$险种投保份数<-as.numeric(data$险种投保份数);summary(data$险种投保份数)
  data$险种投保金额<-as.numeric(data$险种投保金额);summary(data$险种投保金额)
  data$削减保额<-as.numeric(data$削减保额);summary(data$削减保额)
  data$标准体当期保费<-as.numeric(data$标准体当期保费);summary(data$标准体当期保费)
  data$缴费年限或年龄<-as.numeric(data$缴费年限或年龄);summary(data$缴费年限或年龄)
  data$业务员所属二级机构ID<-as.character(data$业务员所属二级机构ID);summary(data$业务员所属二级机构ID)
  data$二次入司标志代码<-as.factor(data$二次入司标志代码);summary(data$二次入司标志代码)
  data$绿色通道授权状态代码<-as.factor(data$绿色通道授权状态代码);summary(data$绿色通道授权状态代码)
  data$客户证件类型ID<-as.factor(data$客户证件类型ID);summary(data$客户证件类型ID)
  data$客户收入<-as.numeric(data$客户收入);summary(data$客户收入)
  data$客户等级代码<-as.factor(data$客户等级代码);summary(data$客户等级代码)
  data$高端客户标识2<-as.factor(data$高端客户标识2);summary(data$高端客户标识2)
  data$规保客户积分<-as.numeric(data$规保客户积分);summary(data$规保客户积分)
  data$险种保额<-as.numeric(data$险种保额);summary(data$险种保额)
  data$立案时效<-as.numeric(data$立案时效);summary(data$立案时效)
  data$住院合计费用<-as.numeric(data$住院合计费用);summary(data$住院合计费用)
  data$住院统筹支付费用<-as.numeric(data$住院统筹支付费用);summary(data$住院统筹支付费用)
  data$住院其他扣除<-as.numeric(data$住院其他扣除);summary(data$住院其他扣除)
  data$住院自费费用<-as.numeric(data$住院自费费用);summary(data$住院自费费用)
  data$住院自付费用<-as.numeric(data$住院自付费用);summary(data$住院自付费用)
  data$门诊合计费用<-as.numeric(data$门诊合计费用);summary(data$门诊合计费用)
  data$门诊统筹支付费用<-as.numeric(data$门诊统筹支付费用);summary(data$门诊统筹支付费用)
  data$门诊其他扣除<-as.numeric(data$门诊其他扣除);summary(data$门诊其他扣除)
  data$门诊自费费用<-as.numeric(data$门诊自费费用);summary(data$门诊自费费用)
  data$门诊自付费用<-as.numeric(data$门诊自付费用);summary(data$门诊自付费用)
  data$被保人医疗险年度保费总额<-as.numeric(data$被保人医疗险年度保费总额);summary(data$被保人医疗险年度保费总额)
  data$责任赔付金额<-as.numeric(data$责任赔付金额);summary(data$责任赔付金额)
  data$发票总金额<-as.numeric(data$发票总金额);summary(data$发票总金额)
  data$自费总金额<-as.numeric(data$自费总金额);summary(data$自费总金额)
  data$其它扣除<-as.numeric(data$其它扣除);summary(data$其它扣除)
  data$床位费.住院<-as.numeric(data$床位费.住院);summary(data$床位费.住院)
  data$手术费.住院<-as.numeric(data$手术费.住院);summary(data$手术费.住院)
  data$西药费.住院<-as.numeric(data$西药费.住院);summary(data$西药费.住院)
  data$检查费.住院<-as.numeric(data$检查费.住院);summary(data$检查费.住院)
  data$特殊检查费.住院<-as.numeric(data$特殊检查费.住院);summary(data$特殊检查费.住院)
  data$护理费.住院<-as.numeric(data$护理费.住院);summary(data$护理费.住院)
  data$中成药费.住院<-as.numeric(data$中成药费.住院);summary(data$中成药费.住院)
  data$化验费.住院<-as.numeric(data$化验费.住院);summary(data$化验费.住院)
  data$中草药费.住院<-as.numeric(data$中草药费.住院);summary(data$中草药费.住院)
  data$放射费.住院<-as.numeric(data$放射费.住院);summary(data$放射费.住院)
  data$诊察费.住院<-as.numeric(data$诊察费.住院);summary(data$诊察费.住院)
  data$输血费.住院<-as.numeric(data$输血费.住院);summary(data$输血费.住院)
  data$材料费.住院<-as.numeric(data$材料费.住院);summary(data$材料费.住院)
  data$其他.住院<-as.numeric(data$其他.住院);summary(data$其他.住院)
  data$西药费.门诊<-as.numeric(data$西药费.门诊);summary(data$西药费.门诊)
  data$检查费.门诊<-as.numeric(data$检查费.门诊);summary(data$检查费.门诊)
  data$手术费.门诊<-as.numeric(data$手术费.门诊);summary(data$手术费.门诊)
  data$中成药费.住院.1<-as.numeric(data$中成药费.住院.1);summary(data$中成药费.住院.1)
  data$化验费.住院.1<-as.numeric(data$化验费.住院.1);summary(data$化验费.住院.1)
  data$治疗费.门诊<-as.numeric(data$治疗费.门诊);summary(data$治疗费.门诊)
  data$中草药费.门诊<-as.numeric(data$中草药费.门诊);summary(data$中草药费.门诊)
  data$放射费.门诊<-as.numeric(data$放射费.门诊);summary(data$放射费.门诊)
  data$诊察费.门诊<-as.numeric(data$诊察费.门诊);summary(data$诊察费.门诊)
  data$特殊检查费.门诊<-as.numeric(data$特殊检查费.门诊);summary(data$特殊检查费.门诊)
  data$材料费.门诊<-as.numeric(data$材料费.门诊);summary(data$材料费.门诊)
  data$其他.门诊<-as.numeric(data$其他.门诊);summary(data$其他.门诊)
  data$在医天数<-as.numeric(data$在医天数);summary(data$在医天数)
  data$历史理赔总额<-as.numeric(data$历史理赔总额);summary(data$历史理赔总额)
  data$案件理赔申请日期减案件出险日期<-as.numeric(data$案件理赔申请日期减案件出险日期);summary(data$案件理赔申请日期减案件出险日期)
  data$代理人离职时间减保单生效日期<-as.numeric(data$代理人离职时间减保单生效日期);summary(data$代理人离职时间减保单生效日期)
  data$案件出险日期减代理人离职时间<-as.numeric(data$案件出险日期减代理人离职时间);summary(data$案件出险日期减代理人离职时间)
  data$案件理赔申请日期减代理人离职时间<-as.numeric(data$案件理赔申请日期减代理人离职时间);summary(data$案件理赔申请日期减代理人离职时间)
  data$案件出险日期减险种生效时间<-as.numeric(data$案件出险日期减险种生效时间);summary(data$案件出险日期减险种生效时间)
  data$案件理赔申请日期减险种生效时间<-as.numeric(data$案件理赔申请日期减险种生效时间);summary(data$案件理赔申请日期减险种生效时间)
  data$业务员年龄<-as.numeric(data$业务员年龄);summary(data$业务员年龄)
  data$业务员入司年限<-as.numeric(data$业务员入司年限);summary(data$业务员入司年限)
  data$是否有工作单位名称<-as.character(data$是否有工作单位名称);summary(data$是否有工作单位名称)
  data$是否有工作单位地址<-as.factor(data$是否有工作单位地址);summary(data$是否有工作单位地址)
  data$是否有客户联系地址<-as.factor(data$是否有客户联系地址);summary(data$是否有客户联系地址)
  data$value残疾保险金<-as.factor(data$value残疾保险金);summary(data$value残疾保险金)
  data$value豁免保险金<-as.factor(data$value豁免保险金);summary(data$value豁免保险金)
  data$value门诊费用<-as.factor(data$value门诊费用);summary(data$value门诊费用)
  data$value烧烫伤保险金<-as.factor(data$value烧烫伤保险金);summary(data$value烧烫伤保险金)
  data$value身故保险金<-as.factor(data$value身故保险金);summary(data$value身故保险金)
  data$value手术费用<-as.factor(data$value手术费用);summary(data$value手术费用)
  data$value特种疾病津贴.x<-as.factor(data$value特种疾病津贴.x);summary(data$value特种疾病津贴.x)
  data$value重大疾病保险金<-as.factor(data$value重大疾病保险金);summary(data$value重大疾病保险金)
  data$value重症监护津贴<-as.factor(data$value重症监护津贴);summary(data$value重症监护津贴)
  data$value住院费用<-as.factor(data$value住院费用);summary(data$value住院费用)
  data$value住院津贴<-as.factor(data$value住院津贴);summary(data$value住院津贴)
  data$value豁免<-as.factor(data$value豁免);summary(data$value豁免)
  data$value疾病末期<-as.factor(data$value疾病末期);summary(data$value疾病末期)
  data$value门诊<-as.factor(data$value门诊);summary(data$value门诊)
  data$value其它<-as.factor(data$value其它);summary(data$value其它)
  data$value烧烫伤<-as.factor(data$value烧烫伤);summary(data$value烧烫伤)
  data$value身故<-as.factor(data$value身故);summary(data$value身故)
  data$value手术<-as.factor(data$value手术);summary(data$value手术)
  data$value特种疾病津贴.y<-as.factor(data$value特种疾病津贴.y);summary(data$value特种疾病津贴.y)
  data$value医疗疾病<-as.factor(data$value医疗疾病);summary(data$value医疗疾病)
  data$value重疾<-as.factor(data$value重疾);summary(data$value重疾)
  data$value重症监护<-as.factor(data$value重症监护);summary(data$value重症监护)
  data$value住院<-as.factor(data$value住院);summary(data$value住院)
  data$value残疾<-as.factor(data$value残疾);summary(data$value残疾)
  data$value101个单投保人变更<-as.factor(data$value101个单投保人变更);summary(data$value101个单投保人变更)
  data$value102客户基本资料变更<-as.factor(data$value102客户基本资料变更);summary(data$value102客户基本资料变更)
  data$value103受益人变更<-as.factor(data$value103受益人变更);summary(data$value103受益人变更)
  data$value108个单新增附约<-as.factor(data$value108个单新增附约);summary(data$value108个单新增附约)
  data$value117职业变更<-as.factor(data$value117职业变更);summary(data$value117职业变更)
  data$value125个单补充告知<-as.factor(data$value125个单补充告知);summary(data$value125个单补充告知)
  data$value161个单生日性别更正<-as.factor(data$value161个单生日性别更正);summary(data$value161个单生日性别更正)
  data$value388停效改有效<-as.factor(data$value388停效改有效);summary(data$value388停效改有效)
  data$value401个单普通复效<-as.factor(data$value401个单普通复效);summary(data$value401个单普通复效)
  data$value415免息复效<-as.factor(data$value415免息复效);summary(data$value415免息复效)
  data$险种责任<-as.factor(data$险种责任);summary(data$险种责任)
  data$案件事故性质<-as.factor(data$案件事故性质);summary(data$案件事故性质)
  data$当前案件理赔申请次数<-as.numeric(data$当前案件理赔申请次数);summary(data$当前案件理赔申请次数)
  data$是否首次申请理赔<-as.factor(data$是否首次申请理赔);summary(data$是否首次申请理赔)
  data$代理人是否在职<-as.factor(data$代理人是否在职);summary(data$代理人是否在职)
  data$保单所属机构代码<-as.character(data$保单所属机构代码);summary(data$保单所属机构代码)
  data$销售渠道<-as.character(data$销售渠道);summary(data$销售渠道)
  data$出单渠道<-as.character(data$出单渠道);summary(data$出单渠道)
  data$保单销售方式<-as.character(data$保单销售方式);summary(data$保单销售方式)
  data$投保人与主被保人关系<-as.factor(data$投保人与主被保人关系);summary(data$投保人与主被保人关系)
  data$保单质押状态<-as.factor(data$保单质押状态);summary(data$保单质押状态)
  data$险种投保档次<-as.factor(data$险种投保档次);summary(data$险种投保档次)
  data$保单险种来源<-as.factor(data$保单险种来源);summary(data$保单险种来源)
  data$险种主附约别<-as.factor(data$险种主附约别);summary(data$险种主附约别)
  data$保障年期类型<-as.factor(data$保障年期类型);summary(data$保障年期类型)
  data$标准保障年限或年龄<-as.numeric(data$标准保障年限或年龄);summary(data$标准保障年限或年龄)
  data$险种失效原因<-as.factor(data$险种失效原因);summary(data$险种失效原因)
  data$险种是否暂停<-as.factor(data$险种是否暂停);summary(data$险种是否暂停)
  data$加费方法<-as.character(data$加费方法);summary(data$加费方法)
  data$保证给付年限<-as.numeric(data$保证给付年限);summary(data$保证给付年限)
  data$险种给付状态<-as.factor(data$险种给付状态);summary(data$险种给付状态)
  data$给付方式<-as.factor(data$给付方式);summary(data$给付方式)
  data$给付结束年期类型<-as.factor(data$给付结束年期类型);summary(data$给付结束年期类型)
  data$给付结束年限或年龄<-as.factor(data$给付结束年限或年龄);summary(data$给付结束年限或年龄)
  data$给付开始年期类型<-as.factor(data$给付开始年期类型);summary(data$给付开始年期类型)
  data$给付开始年限或年龄<-as.factor(data$给付开始年限或年龄);summary(data$给付开始年限或年龄)
  data$当期缴费方式<-as.factor(data$当期缴费方式);summary(data$当期缴费方式)
  data$缴费年期类型<-as.factor(data$缴费年期类型);summary(data$缴费年期类型)
  data$险种缴费年度<-as.numeric(data$险种缴费年度);summary(data$险种缴费年度)
  data$险种缴费状态<-as.factor(data$险种缴费状态);summary(data$险种缴费状态)
  data$险种实际缴费期次<-as.numeric(data$险种实际缴费期次);summary(data$险种实际缴费期次)
  data$保费缴付年期<-as.numeric(data$保费缴付年期);summary(data$保费缴付年期)
  data$职业加费方式<-as.factor(data$职业加费方式);summary(data$职业加费方式)
  data$次标体开始加费类型<-as.factor(data$次标体开始加费类型);summary(data$次标体开始加费类型)
  data$次标体加费比例<-as.factor(data$次标体加费比例);summary(data$次标体加费比例)
  data$次标体加费方式<-as.factor(data$次标体加费方式);summary(data$次标体加费方式)
  data$业务员直属部门代码<-as.character(data$业务员直属部门代码);summary(data$业务员直属部门代码)
  data$业务员直属机构ID<-as.character(data$业务员直属机构ID);summary(data$业务员直属机构ID)
  data$业务员性别<-as.character(data$业务员性别);summary(data$业务员性别)
  data$业务员文化程度<-as.factor(data$业务员文化程度);summary(data$业务员文化程度)
  data$业务员婚姻状况<-as.factor(data$业务员婚姻状况);summary(data$业务员婚姻状况)
  data$业务员状态<-as.factor(data$业务员状态);summary(data$业务员状态)
  data$是否代理人自保件<-as.factor(data$是否代理人自保件);summary(data$是否代理人自保件)
  data$业务员品质职级<-as.factor(data$业务员品质职级);summary(data$业务员品质职级)
  data$业务员分类<-as.character(data$业务员分类);summary(data$业务员分类)
  data$客户性别代码<-as.character(data$客户性别代码);summary(data$客户性别代码)
  data$客户年龄<-as.numeric(data$客户年龄);summary(data$客户年龄)
  data$客户身高<-as.numeric(data$客户身高);summary(data$客户身高)
  data$客户婚姻状况<-as.factor(data$客户婚姻状况);summary(data$客户婚姻状况)
  data$客户是否是一家之主<-as.factor(data$客户是否是一家之主);summary(data$客户是否是一家之主)
  data$职业ID<-as.factor(data$职业ID);summary(data$职业ID)
  data$手机号码等级代码<-as.factor(data$手机号码等级代码);summary(data$手机号码等级代码)
  data$客户等级<-as.factor(data$客户等级);summary(data$客户等级)
  data$是否社保<-as.factor(data$是否社保);summary(data$是否社保)
  data$健康险EM值<-as.numeric(data$健康险EM值);summary(data$健康险EM值)
  data$寿险EM值<-as.numeric(data$寿险EM值);summary(data$寿险EM值)
  data$是否吸烟<-as.factor(data$是否吸烟);summary(data$是否吸烟)
  data$保单寿险保额<-as.numeric(data$保单寿险保额);summary(data$保单寿险保额)
  data$保单意外险保额<-as.numeric(data$保单意外险保额);summary(data$保单意外险保额)
  data$保单人身险保额<-as.numeric(data$保单人身险保额);summary(data$保单人身险保额)
  data$保单重疾险保额<-as.numeric(data$保单重疾险保额);summary(data$保单重疾险保额)
  data$险种重疾保额<-as.numeric(data$险种重疾保额);summary(data$险种重疾保额)
  data$险种人身险保额<-as.numeric(data$险种人身险保额);summary(data$险种人身险保额)
  data$险种意外险保额<-as.numeric(data$险种意外险保额);summary(data$险种意外险保额)
  data$险种重疾险保额<-as.numeric(data$险种重疾险保额);summary(data$险种重疾险保额)
  data$自动核保结论<-as.character(data$自动核保结论);summary(data$自动核保结论)
  data$业务年度<-as.numeric(data$业务年度);summary(data$业务年度)
  data$是否契调<-as.factor(data$是否契调);summary(data$是否契调)
  data$核保限制档次<-as.factor(data$核保限制档次);summary(data$核保限制档次)
  data$险种核保决定结论<-as.factor(data$险种核保决定结论);summary(data$险种核保决定结论)
  data$是否体检<-as.factor(data$是否体检);summary(data$是否体检)
  data$续保决定<-as.factor(data$续保决定);summary(data$续保决定)
  data$案件类型<-as.factor(data$案件类型);summary(data$案件类型)
  data$案件业务类型<-as.factor(data$案件业务类型);summary(data$案件业务类型)
  data$案件业务来源<-as.factor(data$案件业务来源);summary(data$案件业务来源)
  data$诊断结果<-as.factor(data$诊断结果);summary(data$诊断结果)
  data$是否原件或复印件<-as.character(data$是否原件或复印件);summary(data$是否原件或复印件)
  data$此保单年度保费总额<-as.numeric(data$此保单年度保费总额);summary(data$此保单年度保费总额)
  data$住院或门诊<-as.factor(data$住院或门诊);summary(data$住院或门诊)
  data$是否慢性病<-as.factor(data$是否慢性病);summary(data$是否慢性病)
  data$历史理赔案件数<-as.numeric(data$历史理赔案件数);summary(data$历史理赔案件数)
  data$历史理赔险种<-as.factor(data$历史理赔险种);summary(data$历史理赔险种)
  data$是否曾经被认定为欺诈<-as.factor(data$是否曾经被认定为欺诈);summary(data$是否曾经被认定为欺诈)
}

######################################################################
###################2. 计算每个变量的IV/adjusted IV####################
######################################################################

############## 2.1 对每个变量分别构建DATA.FRAME ################
{
  data_险种责任<-as.data.frame(data[c("险种责任","险种理赔拒赔原因")])
  data_案件事故性质<-as.data.frame(data[c("案件事故性质","险种理赔拒赔原因")])
  data_当前案件理赔申请次数<-as.data.frame(data[c("当前案件理赔申请次数","险种理赔拒赔原因")])
  data_是否首次申请理赔<-as.data.frame(data[c("是否首次申请理赔","险种理赔拒赔原因")])
  data_代理人是否在职<-as.data.frame(data[c("代理人是否在职","险种理赔拒赔原因")])
  data_保单所属二级机构代码<-as.data.frame(data[c("保单所属二级机构代码","险种理赔拒赔原因")])
  data_保单所属机构代码<-as.data.frame(data[c("保单所属机构代码","险种理赔拒赔原因")])
  data_销售渠道<-as.data.frame(data[c("销售渠道","险种理赔拒赔原因")])
  data_出单渠道<-as.data.frame(data[c("出单渠道","险种理赔拒赔原因")])
  data_保单销售方式<-as.data.frame(data[c("保单销售方式","险种理赔拒赔原因")])
  data_投保人与主被保人关系<-as.data.frame(data[c("投保人与主被保人关系","险种理赔拒赔原因")])
  data_保单质押状态<-as.data.frame(data[c("保单质押状态","险种理赔拒赔原因")])
  data_险种内部代码<-as.data.frame(data[c("险种内部代码","险种理赔拒赔原因")])
  data_险种投保档次<-as.data.frame(data[c("险种投保档次","险种理赔拒赔原因")])
  data_险种投保份数<-as.data.frame(data[c("险种投保份数","险种理赔拒赔原因")])
  data_保单险种来源<-as.data.frame(data[c("保单险种来源","险种理赔拒赔原因")])
  data_险种主附约别<-as.data.frame(data[c("险种主附约别","险种理赔拒赔原因")])
  data_保障年期类型<-as.data.frame(data[c("保障年期类型","险种理赔拒赔原因")])
  data_标准保障年限或年龄<-as.data.frame(data[c("标准保障年限或年龄","险种理赔拒赔原因")])
  data_险种失效原因<-as.data.frame(data[c("险种失效原因","险种理赔拒赔原因")])
  data_险种是否暂停<-as.data.frame(data[c("险种是否暂停","险种理赔拒赔原因")])
  data_险种投保金额<-as.data.frame(data[c("险种投保金额","险种理赔拒赔原因")])
  data_削减保额<-as.data.frame(data[c("削减保额","险种理赔拒赔原因")])
  data_标准体当期保费<-as.data.frame(data[c("标准体当期保费","险种理赔拒赔原因")])
  data_加费方法<-as.data.frame(data[c("加费方法","险种理赔拒赔原因")])
  data_保证给付年限<-as.data.frame(data[c("保证给付年限","险种理赔拒赔原因")])
  data_险种给付状态<-as.data.frame(data[c("险种给付状态","险种理赔拒赔原因")])
  data_给付方式<-as.data.frame(data[c("给付方式","险种理赔拒赔原因")])
  data_给付结束年期类型<-as.data.frame(data[c("给付结束年期类型","险种理赔拒赔原因")])
  data_给付结束年限或年龄<-as.data.frame(data[c("给付结束年限或年龄","险种理赔拒赔原因")])
  data_给付开始年期类型<-as.data.frame(data[c("给付开始年期类型","险种理赔拒赔原因")])
  data_给付开始年限或年龄<-as.data.frame(data[c("给付开始年限或年龄","险种理赔拒赔原因")])
  data_当期缴费方式<-as.data.frame(data[c("当期缴费方式","险种理赔拒赔原因")])
  data_缴费年期类型<-as.data.frame(data[c("缴费年期类型","险种理赔拒赔原因")])
  data_缴费年限或年龄<-as.data.frame(data[c("缴费年限或年龄","险种理赔拒赔原因")])
  data_险种缴费年度<-as.data.frame(data[c("险种缴费年度","险种理赔拒赔原因")])
  data_险种缴费状态<-as.data.frame(data[c("险种缴费状态","险种理赔拒赔原因")])
  data_险种实际缴费期次<-as.data.frame(data[c("险种实际缴费期次","险种理赔拒赔原因")])
  data_保费缴付年期<-as.data.frame(data[c("保费缴付年期","险种理赔拒赔原因")])
  data_职业加费方式<-as.data.frame(data[c("职业加费方式","险种理赔拒赔原因")])
  data_次标体开始加费类型<-as.data.frame(data[c("次标体开始加费类型","险种理赔拒赔原因")])
  data_次标体加费比例<-as.data.frame(data[c("次标体加费比例","险种理赔拒赔原因")])
  data_次标体加费方式<-as.data.frame(data[c("次标体加费方式","险种理赔拒赔原因")])
  data_业务员直属部门代码<-as.data.frame(data[c("业务员直属部门代码","险种理赔拒赔原因")])
  data_业务员直属机构ID<-as.data.frame(data[c("业务员直属机构ID","险种理赔拒赔原因")])
  data_业务员所属二级机构ID<-as.data.frame(data[c("业务员所属二级机构ID","险种理赔拒赔原因")])
  data_业务员性别<-as.data.frame(data[c("业务员性别","险种理赔拒赔原因")])
  data_业务员文化程度<-as.data.frame(data[c("业务员文化程度","险种理赔拒赔原因")])
  data_业务员婚姻状况<-as.data.frame(data[c("业务员婚姻状况","险种理赔拒赔原因")])
  data_业务员状态<-as.data.frame(data[c("业务员状态","险种理赔拒赔原因")])
  data_是否代理人自保件<-as.data.frame(data[c("是否代理人自保件","险种理赔拒赔原因")])
  data_业务员品质职级<-as.data.frame(data[c("业务员品质职级","险种理赔拒赔原因")])
  data_业务员分类<-as.data.frame(data[c("业务员分类","险种理赔拒赔原因")])
  data_二次入司标志代码<-as.data.frame(data[c("二次入司标志代码","险种理赔拒赔原因")])
  data_绿色通道授权状态代码<-as.data.frame(data[c("绿色通道授权状态代码","险种理赔拒赔原因")])
  data_客户性别代码<-as.data.frame(data[c("客户性别代码","险种理赔拒赔原因")])
  data_客户身高<-as.data.frame(data[c("客户身高","险种理赔拒赔原因")])
  data_客户证件类型ID<-as.data.frame(data[c("客户证件类型ID","险种理赔拒赔原因")])
  data_客户收入<-as.data.frame(data[c("客户收入","险种理赔拒赔原因")])
  data_客户婚姻状况<-as.data.frame(data[c("客户婚姻状况","险种理赔拒赔原因")])
  data_客户是否是一家之主<-as.data.frame(data[c("客户是否是一家之主","险种理赔拒赔原因")])
  data_职业ID<-as.data.frame(data[c("职业ID","险种理赔拒赔原因")])
  data_手机号码等级代码<-as.data.frame(data[c("手机号码等级代码","险种理赔拒赔原因")])
  data_客户等级代码<-as.data.frame(data[c("客户等级代码","险种理赔拒赔原因")])
  data_客户等级<-as.data.frame(data[c("客户等级","险种理赔拒赔原因")])
  data_高端客户标识2<-as.data.frame(data[c("高端客户标识2","险种理赔拒赔原因")])
  data_规保客户积分<-as.data.frame(data[c("规保客户积分","险种理赔拒赔原因")])
  data_是否社保<-as.data.frame(data[c("是否社保","险种理赔拒赔原因")])
  data_健康险EM值<-as.data.frame(data[c("健康险EM值","险种理赔拒赔原因")])
  data_寿险EM值<-as.data.frame(data[c("寿险EM值","险种理赔拒赔原因")])
  data_是否吸烟<-as.data.frame(data[c("是否吸烟","险种理赔拒赔原因")])
  data_保单寿险保额<-as.data.frame(data[c("保单寿险保额","险种理赔拒赔原因")])
  data_保单意外险保额<-as.data.frame(data[c("保单意外险保额","险种理赔拒赔原因")])
  data_保单人身险保额<-as.data.frame(data[c("保单人身险保额","险种理赔拒赔原因")])
  data_保单重疾险保额<-as.data.frame(data[c("保单重疾险保额","险种理赔拒赔原因")])
  data_险种重疾保额<-as.data.frame(data[c("险种重疾保额","险种理赔拒赔原因")])
  data_险种人身险保额<-as.data.frame(data[c("险种人身险保额","险种理赔拒赔原因")])
  data_险种意外险保额<-as.data.frame(data[c("险种意外险保额","险种理赔拒赔原因")])
  data_险种重疾险保额<-as.data.frame(data[c("险种重疾险保额","险种理赔拒赔原因")])
  data_险种保额<-as.data.frame(data[c("险种保额","险种理赔拒赔原因")])
  data_自动核保结论<-as.data.frame(data[c("自动核保结论","险种理赔拒赔原因")])
  data_业务年度<-as.data.frame(data[c("业务年度","险种理赔拒赔原因")])
  data_是否契调<-as.data.frame(data[c("是否契调","险种理赔拒赔原因")])
  data_核保限制档次<-as.data.frame(data[c("核保限制档次","险种理赔拒赔原因")])
  data_险种核保决定结论<-as.data.frame(data[c("险种核保决定结论","险种理赔拒赔原因")])
  data_是否体检<-as.data.frame(data[c("是否体检","险种理赔拒赔原因")])
  data_续保决定<-as.data.frame(data[c("续保决定","险种理赔拒赔原因")])
  data_案件类型<-as.data.frame(data[c("案件类型","险种理赔拒赔原因")])
  data_案件业务类型<-as.data.frame(data[c("案件业务类型","险种理赔拒赔原因")])
  data_案件业务来源<-as.data.frame(data[c("案件业务来源","险种理赔拒赔原因")])
  data_诊断结果<-as.data.frame(data[c("诊断结果","险种理赔拒赔原因")])
  data_立案时效<-as.data.frame(data[c("立案时效","险种理赔拒赔原因")])
  data_是否原件或复印件<-as.data.frame(data[c("是否原件或复印件","险种理赔拒赔原因")])
  data_住院合计费用<-as.data.frame(data[c("住院合计费用","险种理赔拒赔原因")])
  data_住院统筹支付费用<-as.data.frame(data[c("住院统筹支付费用","险种理赔拒赔原因")])
  data_住院其他扣除<-as.data.frame(data[c("住院其他扣除","险种理赔拒赔原因")])
  data_住院自费费用<-as.data.frame(data[c("住院自费费用","险种理赔拒赔原因")])
  data_住院自付费用<-as.data.frame(data[c("住院自付费用","险种理赔拒赔原因")])
  data_门诊合计费用<-as.data.frame(data[c("门诊合计费用","险种理赔拒赔原因")])
  data_门诊统筹支付费用<-as.data.frame(data[c("门诊统筹支付费用","险种理赔拒赔原因")])
  data_门诊其他扣除<-as.data.frame(data[c("门诊其他扣除","险种理赔拒赔原因")])
  data_门诊自费费用<-as.data.frame(data[c("门诊自费费用","险种理赔拒赔原因")])
  data_门诊自付费用<-as.data.frame(data[c("门诊自付费用","险种理赔拒赔原因")])
  data_被保人医疗险年度保费总额<-as.data.frame(data[c("被保人医疗险年度保费总额","险种理赔拒赔原因")])
  data_此保单年度保费总额<-as.data.frame(data[c("此保单年度保费总额","险种理赔拒赔原因")])
  data_责任赔付金额<-as.data.frame(data[c("责任赔付金额","险种理赔拒赔原因")])
  data_发票总金额<-as.data.frame(data[c("发票总金额","险种理赔拒赔原因")])
  data_自费总金额<-as.data.frame(data[c("自费总金额","险种理赔拒赔原因")])
  data_其它扣除<-as.data.frame(data[c("其它扣除","险种理赔拒赔原因")])
  data_床位费.住院<-as.data.frame(data[c("床位费.住院","险种理赔拒赔原因")])
  data_手术费.住院<-as.data.frame(data[c("手术费.住院","险种理赔拒赔原因")])
  data_西药费.住院<-as.data.frame(data[c("西药费.住院","险种理赔拒赔原因")])
  data_检查费.住院<-as.data.frame(data[c("检查费.住院","险种理赔拒赔原因")])
  data_特殊检查费.住院<-as.data.frame(data[c("特殊检查费.住院","险种理赔拒赔原因")])
  data_护理费.住院<-as.data.frame(data[c("护理费.住院","险种理赔拒赔原因")])
  data_中成药费.住院<-as.data.frame(data[c("中成药费.住院","险种理赔拒赔原因")])
  data_化验费.住院<-as.data.frame(data[c("化验费.住院","险种理赔拒赔原因")])
  data_中草药费.住院<-as.data.frame(data[c("中草药费.住院","险种理赔拒赔原因")])
  data_放射费.住院<-as.data.frame(data[c("放射费.住院","险种理赔拒赔原因")])
  data_诊察费.住院<-as.data.frame(data[c("诊察费.住院","险种理赔拒赔原因")])
  data_输血费.住院<-as.data.frame(data[c("输血费.住院","险种理赔拒赔原因")])
  data_材料费.住院<-as.data.frame(data[c("材料费.住院","险种理赔拒赔原因")])
  data_其他.住院<-as.data.frame(data[c("其他.住院","险种理赔拒赔原因")])
  data_西药费.门诊<-as.data.frame(data[c("西药费.门诊","险种理赔拒赔原因")])
  data_检查费.门诊<-as.data.frame(data[c("检查费.门诊","险种理赔拒赔原因")])
  data_手术费.门诊<-as.data.frame(data[c("手术费.门诊","险种理赔拒赔原因")])
  data_中成药费.住院.1<-as.data.frame(data[c("中成药费.住院.1","险种理赔拒赔原因")])
  data_化验费.住院.1<-as.data.frame(data[c("化验费.住院.1","险种理赔拒赔原因")])
  data_治疗费.门诊<-as.data.frame(data[c("治疗费.门诊","险种理赔拒赔原因")])
  data_中草药费.门诊<-as.data.frame(data[c("中草药费.门诊","险种理赔拒赔原因")])
  data_放射费.门诊<-as.data.frame(data[c("放射费.门诊","险种理赔拒赔原因")])
  data_诊察费.门诊<-as.data.frame(data[c("诊察费.门诊","险种理赔拒赔原因")])
  data_特殊检查费.门诊<-as.data.frame(data[c("特殊检查费.门诊","险种理赔拒赔原因")])
  data_材料费.门诊<-as.data.frame(data[c("材料费.门诊","险种理赔拒赔原因")])
  data_其他.门诊<-as.data.frame(data[c("其他.门诊","险种理赔拒赔原因")])
  data_住院或门诊<-as.data.frame(data[c("住院或门诊","险种理赔拒赔原因")])
  data_在医天数<-as.data.frame(data[c("在医天数","险种理赔拒赔原因")])
  data_是否慢性病<-as.data.frame(data[c("是否慢性病","险种理赔拒赔原因")])
  data_历史理赔案件数<-as.data.frame(data[c("历史理赔案件数","险种理赔拒赔原因")])
  data_历史理赔总额<-as.data.frame(data[c("历史理赔总额","险种理赔拒赔原因")])
  data_历史理赔险种<-as.data.frame(data[c("历史理赔险种","险种理赔拒赔原因")])
  data_是否曾经被认定为欺诈<-as.data.frame(data[c("是否曾经被认定为欺诈","险种理赔拒赔原因")])
  data_案件理赔申请日期减案件出险日期<-as.data.frame(data[c("案件理赔申请日期减案件出险日期","险种理赔拒赔原因")])
  data_代理人离职时间减保单生效日期<-as.data.frame(data[c("代理人离职时间减保单生效日期","险种理赔拒赔原因")])
  data_案件出险日期减代理人离职时间<-as.data.frame(data[c("案件出险日期减代理人离职时间","险种理赔拒赔原因")])
  data_案件理赔申请日期减代理人离职时间<-as.data.frame(data[c("案件理赔申请日期减代理人离职时间","险种理赔拒赔原因")])
  data_案件出险日期减险种生效时间<-as.data.frame(data[c("案件出险日期减险种生效时间","险种理赔拒赔原因")])
  data_案件理赔申请日期减险种生效时间<-as.data.frame(data[c("案件理赔申请日期减险种生效时间","险种理赔拒赔原因")])
  data_业务员年龄<-as.data.frame(data[c("业务员年龄","险种理赔拒赔原因")])
  data_业务员入司年限<-as.data.frame(data[c("业务员入司年限","险种理赔拒赔原因")])
  data_客户年龄<-as.data.frame(data[c("客户年龄","险种理赔拒赔原因")])
  data_是否有工作单位名称<-as.data.frame(data[c("是否有工作单位名称","险种理赔拒赔原因")])
  data_是否有工作单位地址<-as.data.frame(data[c("是否有工作单位地址","险种理赔拒赔原因")])
  data_是否有客户联系地址<-as.data.frame(data[c("是否有客户联系地址","险种理赔拒赔原因")])
  data_value残疾保险金<-as.data.frame(data[c("value残疾保险金","险种理赔拒赔原因")])
  data_value豁免保险金<-as.data.frame(data[c("value豁免保险金","险种理赔拒赔原因")])
  data_value门诊费用<-as.data.frame(data[c("value门诊费用","险种理赔拒赔原因")])
  data_value烧烫伤保险金<-as.data.frame(data[c("value烧烫伤保险金","险种理赔拒赔原因")])
  data_value身故保险金<-as.data.frame(data[c("value身故保险金","险种理赔拒赔原因")])
  data_value手术费用<-as.data.frame(data[c("value手术费用","险种理赔拒赔原因")])
  data_value特种疾病津贴.x<-as.data.frame(data[c("value特种疾病津贴.x","险种理赔拒赔原因")])
  data_value重大疾病保险金<-as.data.frame(data[c("value重大疾病保险金","险种理赔拒赔原因")])
  data_value重症监护津贴<-as.data.frame(data[c("value重症监护津贴","险种理赔拒赔原因")])
  data_value住院费用<-as.data.frame(data[c("value住院费用","险种理赔拒赔原因")])
  data_value住院津贴<-as.data.frame(data[c("value住院津贴","险种理赔拒赔原因")])
  data_value豁免<-as.data.frame(data[c("value豁免","险种理赔拒赔原因")])
  data_value疾病末期<-as.data.frame(data[c("value疾病末期","险种理赔拒赔原因")])
  data_value门诊<-as.data.frame(data[c("value门诊","险种理赔拒赔原因")])
  data_value其它<-as.data.frame(data[c("value其它","险种理赔拒赔原因")])
  data_value烧烫伤<-as.data.frame(data[c("value烧烫伤","险种理赔拒赔原因")])
  data_value身故<-as.data.frame(data[c("value身故","险种理赔拒赔原因")])
  data_value手术<-as.data.frame(data[c("value手术","险种理赔拒赔原因")])
  data_value特种疾病津贴.y<-as.data.frame(data[c("value特种疾病津贴.y","险种理赔拒赔原因")])
  data_value医疗疾病<-as.data.frame(data[c("value医疗疾病","险种理赔拒赔原因")])
  data_value重疾<-as.data.frame(data[c("value重疾","险种理赔拒赔原因")])
  data_value重症监护<-as.data.frame(data[c("value重症监护","险种理赔拒赔原因")])
  data_value住院<-as.data.frame(data[c("value住院","险种理赔拒赔原因")])
  data_value残疾<-as.data.frame(data[c("value残疾","险种理赔拒赔原因")])
  data_value101个单投保人变更<-as.data.frame(data[c("value101个单投保人变更","险种理赔拒赔原因")])
  data_value102客户基本资料变更<-as.data.frame(data[c("value102客户基本资料变更","险种理赔拒赔原因")])
  data_value103受益人变更<-as.data.frame(data[c("value103受益人变更","险种理赔拒赔原因")])
  data_value108个单新增附约<-as.data.frame(data[c("value108个单新增附约","险种理赔拒赔原因")])
  data_value117职业变更<-as.data.frame(data[c("value117职业变更","险种理赔拒赔原因")])
  data_value125个单补充告知<-as.data.frame(data[c("value125个单补充告知","险种理赔拒赔原因")])
  data_value161个单生日性别更正<-as.data.frame(data[c("value161个单生日性别更正","险种理赔拒赔原因")])
  data_value388停效改有效<-as.data.frame(data[c("value388停效改有效","险种理赔拒赔原因")])
  data_value401个单普通复效<-as.data.frame(data[c("value401个单普通复效","险种理赔拒赔原因")])
  data_value415免息复效<-as.data.frame(data[c("value415免息复效","险种理赔拒赔原因")])
}

################### 2.2 计算每个变量的IV值 #####################
#使用woebin函数计算IV值等
{
  bins_险种责任 <- woebin(data_险种责任, y="险种理赔拒赔原因");bins_险种责任
  bins_案件事故性质 <- woebin(data_案件事故性质, y="险种理赔拒赔原因");bins_案件事故性质
  bins_当前案件理赔申请次数 <- woebin(data_当前案件理赔申请次数, y="险种理赔拒赔原因");bins_当前案件理赔申请次数
  bins_是否首次申请理赔 <- woebin(data_是否首次申请理赔, y="险种理赔拒赔原因");bins_是否首次申请理赔
  bins_代理人是否在职 <- woebin(data_代理人是否在职, y="险种理赔拒赔原因");bins_代理人是否在职
  bins_保单所属二级机构代码 <- woebin(data_保单所属二级机构代码, y="险种理赔拒赔原因");bins_保单所属二级机构代码
  bins_保单所属机构代码 <- woebin(data_保单所属机构代码, y="险种理赔拒赔原因");bins_保单所属机构代码
  bins_销售渠道 <- woebin(data_销售渠道, y="险种理赔拒赔原因");bins_销售渠道
  bins_出单渠道 <- woebin(data_出单渠道, y="险种理赔拒赔原因");bins_出单渠道
  bins_保单销售方式 <- woebin(data_保单销售方式, y="险种理赔拒赔原因");bins_保单销售方式
  bins_投保人与主被保人关系 <- woebin(data_投保人与主被保人关系, y="险种理赔拒赔原因");bins_投保人与主被保人关系
  bins_保单质押状态 <- woebin(data_保单质押状态, y="险种理赔拒赔原因");bins_保单质押状态
  bins_险种内部代码 <- woebin(data_险种内部代码, y="险种理赔拒赔原因");bins_险种内部代码
  bins_险种投保档次 <- woebin(data_险种投保档次, y="险种理赔拒赔原因");bins_险种投保档次
  bins_险种投保份数 <- woebin(data_险种投保份数, y="险种理赔拒赔原因");bins_险种投保份数
  bins_保单险种来源 <- woebin(data_保单险种来源, y="险种理赔拒赔原因");bins_保单险种来源
  bins_险种主附约别 <- woebin(data_险种主附约别, y="险种理赔拒赔原因");bins_险种主附约别
  bins_保障年期类型 <- woebin(data_保障年期类型, y="险种理赔拒赔原因");bins_保障年期类型
  bins_标准保障年限或年龄 <- woebin(data_标准保障年限或年龄, y="险种理赔拒赔原因");bins_标准保障年限或年龄
  bins_险种失效原因 <- woebin(data_险种失效原因, y="险种理赔拒赔原因");bins_险种失效原因
  bins_险种是否暂停 <- woebin(data_险种是否暂停, y="险种理赔拒赔原因");bins_险种是否暂停
  bins_险种投保金额 <- woebin(data_险种投保金额, y="险种理赔拒赔原因");bins_险种投保金额
  bins_削减保额 <- woebin(data_削减保额, y="险种理赔拒赔原因");bins_削减保额
  bins_标准体当期保费 <- woebin(data_标准体当期保费, y="险种理赔拒赔原因");bins_标准体当期保费
  bins_加费方法 <- woebin(data_加费方法, y="险种理赔拒赔原因");bins_加费方法
  bins_保证给付年限 <- woebin(data_保证给付年限, y="险种理赔拒赔原因");bins_保证给付年限
  bins_险种给付状态 <- woebin(data_险种给付状态, y="险种理赔拒赔原因");bins_险种给付状态
  bins_给付方式 <- woebin(data_给付方式, y="险种理赔拒赔原因");bins_给付方式
  bins_给付结束年期类型 <- woebin(data_给付结束年期类型, y="险种理赔拒赔原因");bins_给付结束年期类型
  bins_给付结束年限或年龄 <- woebin(data_给付结束年限或年龄, y="险种理赔拒赔原因");bins_给付结束年限或年龄
  bins_给付开始年期类型 <- woebin(data_给付开始年期类型, y="险种理赔拒赔原因");bins_给付开始年期类型
  bins_给付开始年限或年龄 <- woebin(data_给付开始年限或年龄, y="险种理赔拒赔原因");bins_给付开始年限或年龄
  bins_当期缴费方式 <- woebin(data_当期缴费方式, y="险种理赔拒赔原因");bins_当期缴费方式
  bins_缴费年期类型 <- woebin(data_缴费年期类型, y="险种理赔拒赔原因");bins_缴费年期类型
  bins_缴费年限或年龄 <- woebin(data_缴费年限或年龄, y="险种理赔拒赔原因");bins_缴费年限或年龄
  bins_险种缴费年度 <- woebin(data_险种缴费年度, y="险种理赔拒赔原因");bins_险种缴费年度
  bins_险种缴费状态 <- woebin(data_险种缴费状态, y="险种理赔拒赔原因");bins_险种缴费状态
  bins_险种实际缴费期次 <- woebin(data_险种实际缴费期次, y="险种理赔拒赔原因");bins_险种实际缴费期次
  bins_保费缴付年期 <- woebin(data_保费缴付年期, y="险种理赔拒赔原因");bins_保费缴付年期
  bins_职业加费方式 <- woebin(data_职业加费方式, y="险种理赔拒赔原因");bins_职业加费方式
  bins_次标体开始加费类型 <- woebin(data_次标体开始加费类型, y="险种理赔拒赔原因");bins_次标体开始加费类型
  bins_次标体加费比例 <- woebin(data_次标体加费比例, y="险种理赔拒赔原因");bins_次标体加费比例
  bins_次标体加费方式 <- woebin(data_次标体加费方式, y="险种理赔拒赔原因");bins_次标体加费方式
  ################ bins_业务员直属部门代码 <- woebin(data_业务员直属部门代码, y="险种理赔拒赔原因");bins_业务员直属部门代码
  bins_业务员直属机构ID <- woebin(data_业务员直属机构ID, y="险种理赔拒赔原因");bins_业务员直属机构ID
  bins_业务员所属二级机构ID <- woebin(data_业务员所属二级机构ID, y="险种理赔拒赔原因");bins_业务员所属二级机构ID
  bins_业务员性别 <- woebin(data_业务员性别, y="险种理赔拒赔原因");bins_业务员性别
  bins_业务员文化程度 <- woebin(data_业务员文化程度, y="险种理赔拒赔原因");bins_业务员文化程度
  bins_业务员婚姻状况 <- woebin(data_业务员婚姻状况, y="险种理赔拒赔原因");bins_业务员婚姻状况
  bins_业务员状态 <- woebin(data_业务员状态, y="险种理赔拒赔原因");bins_业务员状态
  bins_是否代理人自保件 <- woebin(data_是否代理人自保件, y="险种理赔拒赔原因");bins_是否代理人自保件
  bins_业务员品质职级 <- woebin(data_业务员品质职级, y="险种理赔拒赔原因");bins_业务员品质职级
  bins_业务员分类 <- woebin(data_业务员分类, y="险种理赔拒赔原因");bins_业务员分类
  bins_二次入司标志代码 <- woebin(data_二次入司标志代码, y="险种理赔拒赔原因");bins_二次入司标志代码
  bins_绿色通道授权状态代码 <- woebin(data_绿色通道授权状态代码, y="险种理赔拒赔原因");bins_绿色通道授权状态代码
  bins_客户性别代码 <- woebin(data_客户性别代码, y="险种理赔拒赔原因");bins_客户性别代码
  bins_客户身高 <- woebin(data_客户身高, y="险种理赔拒赔原因");bins_客户身高
  bins_客户证件类型ID <- woebin(data_客户证件类型ID, y="险种理赔拒赔原因");bins_客户证件类型ID
  bins_客户收入 <- woebin(data_客户收入, y="险种理赔拒赔原因");bins_客户收入
  bins_客户婚姻状况 <- woebin(data_客户婚姻状况, y="险种理赔拒赔原因");bins_客户婚姻状况
  bins_客户是否是一家之主 <- woebin(data_客户是否是一家之主, y="险种理赔拒赔原因");bins_客户是否是一家之主
  bins_职业ID <- woebin(data_职业ID, y="险种理赔拒赔原因");bins_职业ID
  bins_手机号码等级代码 <- woebin(data_手机号码等级代码, y="险种理赔拒赔原因");bins_手机号码等级代码
  ################ bins_客户等级代码 <- woebin(data_客户等级代码, y="险种理赔拒赔原因");bins_客户等级代码
  ################ bins_客户等级 <- woebin(data_客户等级, y="险种理赔拒赔原因");bins_客户等级
  bins_高端客户标识2 <- woebin(data_高端客户标识2, y="险种理赔拒赔原因");bins_高端客户标识2
  bins_规保客户积分 <- woebin(data_规保客户积分, y="险种理赔拒赔原因");bins_规保客户积分
  bins_是否社保 <- woebin(data_是否社保, y="险种理赔拒赔原因");bins_是否社保;class(data_是否社保)
  bins_健康险EM值 <- woebin(data_健康险EM值, y="险种理赔拒赔原因");bins_健康险EM值
  bins_寿险EM值 <- woebin(data_寿险EM值, y="险种理赔拒赔原因");bins_寿险EM值
  bins_是否吸烟 <- woebin(data_是否吸烟, y="险种理赔拒赔原因");bins_是否吸烟
  bins_保单寿险保额 <- woebin(data_保单寿险保额, y="险种理赔拒赔原因");bins_保单寿险保额
  bins_保单意外险保额 <- woebin(data_保单意外险保额, y="险种理赔拒赔原因");bins_保单意外险保额
  bins_保单人身险保额 <- woebin(data_保单人身险保额, y="险种理赔拒赔原因");bins_保单人身险保额
  bins_保单重疾险保额 <- woebin(data_保单重疾险保额, y="险种理赔拒赔原因");bins_保单重疾险保额
  bins_险种重疾保额 <- woebin(data_险种重疾保额, y="险种理赔拒赔原因");bins_险种重疾保额
  bins_险种人身险保额 <- woebin(data_险种人身险保额, y="险种理赔拒赔原因");bins_险种人身险保额
  bins_险种意外险保额 <- woebin(data_险种意外险保额, y="险种理赔拒赔原因");bins_险种意外险保额
  bins_险种重疾险保额 <- woebin(data_险种重疾险保额, y="险种理赔拒赔原因");bins_险种重疾险保额
  bins_险种保额 <- woebin(data_险种保额, y="险种理赔拒赔原因");bins_险种保额
  bins_自动核保结论 <- woebin(data_自动核保结论, y="险种理赔拒赔原因");bins_自动核保结论
  bins_业务年度 <- woebin(data_业务年度, y="险种理赔拒赔原因");bins_业务年度
  bins_是否契调 <- woebin(data_是否契调, y="险种理赔拒赔原因");bins_是否契调
  ############## bins_核保限制档次 <- woebin(data_核保限制档次, y="险种理赔拒赔原因");bins_核保限制档次
  bins_险种核保决定结论 <- woebin(data_险种核保决定结论, y="险种理赔拒赔原因");bins_险种核保决定结论
  bins_是否体检 <- woebin(data_是否体检, y="险种理赔拒赔原因");bins_是否体检
  bins_续保决定 <- woebin(data_续保决定, y="险种理赔拒赔原因");bins_续保决定
  bins_案件类型 <- woebin(data_案件类型, y="险种理赔拒赔原因");bins_案件类型
  bins_案件业务类型 <- woebin(data_案件业务类型, y="险种理赔拒赔原因");bins_案件业务类型
  bins_案件业务来源 <- woebin(data_案件业务来源, y="险种理赔拒赔原因");bins_案件业务来源
  ############## bins_诊断结果 <- woebin(data_诊断结果, y="险种理赔拒赔原因");bins_诊断结果
  bins_立案时效 <- woebin(data_立案时效, y="险种理赔拒赔原因");bins_立案时效
  bins_是否原件或复印件 <- woebin(data_是否原件或复印件, y="险种理赔拒赔原因");bins_是否原件或复印件
  bins_住院合计费用 <- woebin(data_住院合计费用, y="险种理赔拒赔原因");bins_住院合计费用
  bins_住院统筹支付费用 <- woebin(data_住院统筹支付费用, y="险种理赔拒赔原因");bins_住院统筹支付费用
  bins_住院其他扣除 <- woebin(data_住院其他扣除, y="险种理赔拒赔原因");bins_住院其他扣除
  bins_住院自费费用 <- woebin(data_住院自费费用, y="险种理赔拒赔原因");bins_住院自费费用
  bins_住院自付费用 <- woebin(data_住院自付费用, y="险种理赔拒赔原因");bins_住院自付费用
  bins_门诊合计费用 <- woebin(data_门诊合计费用, y="险种理赔拒赔原因");bins_门诊合计费用
  bins_门诊统筹支付费用 <- woebin(data_门诊统筹支付费用, y="险种理赔拒赔原因");bins_门诊统筹支付费用
  bins_门诊其他扣除 <- woebin(data_门诊其他扣除, y="险种理赔拒赔原因");bins_门诊其他扣除
  bins_门诊自费费用 <- woebin(data_门诊自费费用, y="险种理赔拒赔原因");bins_门诊自费费用
  bins_门诊自付费用 <- woebin(data_门诊自付费用, y="险种理赔拒赔原因");bins_门诊自付费用
  bins_被保人医疗险年度保费总额 <- woebin(data_被保人医疗险年度保费总额, y="险种理赔拒赔原因");bins_被保人医疗险年度保费总额
  bins_此保单年度保费总额 <- woebin(data_此保单年度保费总额, y="险种理赔拒赔原因");bins_此保单年度保费总额
  bins_责任赔付金额 <- woebin(data_责任赔付金额, y="险种理赔拒赔原因");bins_责任赔付金额
  bins_发票总金额 <- woebin(data_发票总金额, y="险种理赔拒赔原因");bins_发票总金额
  bins_自费总金额 <- woebin(data_自费总金额, y="险种理赔拒赔原因");bins_自费总金额
  bins_其它扣除 <- woebin(data_其它扣除, y="险种理赔拒赔原因");bins_其它扣除
  bins_床位费.住院 <- woebin(data_床位费.住院, y="险种理赔拒赔原因");bins_床位费.住院
  bins_手术费.住院 <- woebin(data_手术费.住院, y="险种理赔拒赔原因");bins_手术费.住院
  bins_西药费.住院 <- woebin(data_西药费.住院, y="险种理赔拒赔原因");bins_西药费.住院
  bins_检查费.住院 <- woebin(data_检查费.住院, y="险种理赔拒赔原因");bins_检查费.住院
  bins_特殊检查费.住院 <- woebin(data_特殊检查费.住院, y="险种理赔拒赔原因");bins_特殊检查费.住院
  bins_护理费.住院 <- woebin(data_护理费.住院, y="险种理赔拒赔原因");bins_护理费.住院
  bins_中成药费.住院 <- woebin(data_中成药费.住院, y="险种理赔拒赔原因");bins_中成药费.住院
  bins_化验费.住院 <- woebin(data_化验费.住院, y="险种理赔拒赔原因");bins_化验费.住院
  bins_中草药费.住院 <- woebin(data_中草药费.住院, y="险种理赔拒赔原因");bins_中草药费.住院
  bins_放射费.住院 <- woebin(data_放射费.住院, y="险种理赔拒赔原因");bins_放射费.住院
  bins_诊察费.住院 <- woebin(data_诊察费.住院, y="险种理赔拒赔原因");bins_诊察费.住院
  bins_输血费.住院 <- woebin(data_输血费.住院, y="险种理赔拒赔原因");bins_输血费.住院
  bins_材料费.住院 <- woebin(data_材料费.住院, y="险种理赔拒赔原因");bins_材料费.住院
  bins_其他.住院 <- woebin(data_其他.住院, y="险种理赔拒赔原因");bins_其他.住院
  bins_西药费.门诊 <- woebin(data_西药费.门诊, y="险种理赔拒赔原因");bins_西药费.门诊
  bins_检查费.门诊 <- woebin(data_检查费.门诊, y="险种理赔拒赔原因");bins_检查费.门诊
  bins_手术费.门诊 <- woebin(data_手术费.门诊, y="险种理赔拒赔原因");bins_手术费.门诊
  bins_中成药费.住院.1 <- woebin(data_中成药费.住院.1, y="险种理赔拒赔原因");bins_中成药费.住院.1
  bins_化验费.住院.1 <- woebin(data_化验费.住院.1, y="险种理赔拒赔原因");bins_化验费.住院.1
  bins_治疗费.门诊 <- woebin(data_治疗费.门诊, y="险种理赔拒赔原因");bins_治疗费.门诊
  bins_中草药费.门诊 <- woebin(data_中草药费.门诊, y="险种理赔拒赔原因");bins_中草药费.门诊
  bins_放射费.门诊 <- woebin(data_放射费.门诊, y="险种理赔拒赔原因");bins_放射费.门诊
  bins_诊察费.门诊 <- woebin(data_诊察费.门诊, y="险种理赔拒赔原因");bins_诊察费.门诊
  bins_特殊检查费.门诊 <- woebin(data_特殊检查费.门诊, y="险种理赔拒赔原因");bins_特殊检查费.门诊
  bins_材料费.门诊 <- woebin(data_材料费.门诊, y="险种理赔拒赔原因");bins_材料费.门诊
  bins_其他.门诊 <- woebin(data_其他.门诊, y="险种理赔拒赔原因");bins_其他.门诊
  ############# bins_住院或门诊 <- woebin(data_住院或门诊, y="险种理赔拒赔原因");bins_住院或门诊
  bins_在医天数 <- woebin(data_在医天数, y="险种理赔拒赔原因");bins_在医天数
  bins_是否慢性病 <- woebin(data_是否慢性病, y="险种理赔拒赔原因");bins_是否慢性病
  bins_历史理赔案件数 <- woebin(data_历史理赔案件数, y="险种理赔拒赔原因");bins_历史理赔案件数
  bins_历史理赔总额 <- woebin(data_历史理赔总额, y="险种理赔拒赔原因");bins_历史理赔总额
  ############# bins_历史理赔险种 <- woebin(data_历史理赔险种, y="险种理赔拒赔原因");bins_历史理赔险种
  bins_是否曾经被认定为欺诈 <- woebin(data_是否曾经被认定为欺诈, y="险种理赔拒赔原因");bins_是否曾经被认定为欺诈
  bins_案件理赔申请日期减案件出险日期 <- woebin(data_案件理赔申请日期减案件出险日期, y="险种理赔拒赔原因");bins_案件理赔申请日期减案件出险日期
  bins_代理人离职时间减保单生效日期 <- woebin(data_代理人离职时间减保单生效日期, y="险种理赔拒赔原因");bins_代理人离职时间减保单生效日期
  bins_案件出险日期减代理人离职时间 <- woebin(data_案件出险日期减代理人离职时间, y="险种理赔拒赔原因");bins_案件出险日期减代理人离职时间
  bins_案件理赔申请日期减代理人离职时间 <- woebin(data_案件理赔申请日期减代理人离职时间, y="险种理赔拒赔原因");bins_案件理赔申请日期减代理人离职时间
  bins_案件出险日期减险种生效时间 <- woebin(data_案件出险日期减险种生效时间, y="险种理赔拒赔原因");bins_案件出险日期减险种生效时间
  bins_案件理赔申请日期减险种生效时间 <- woebin(data_案件理赔申请日期减险种生效时间, y="险种理赔拒赔原因");bins_案件理赔申请日期减险种生效时间
  bins_业务员年龄 <- woebin(data_业务员年龄, y="险种理赔拒赔原因");bins_业务员年龄
  bins_业务员入司年限 <- woebin(data_业务员入司年限, y="险种理赔拒赔原因");bins_业务员入司年限
  bins_客户年龄 <- woebin(data_客户年龄, y="险种理赔拒赔原因");bins_客户年龄
  bins_是否有工作单位名称 <- woebin(data_是否有工作单位名称, y="险种理赔拒赔原因");bins_是否有工作单位名称
  bins_是否有工作单位地址 <- woebin(data_是否有工作单位地址, y="险种理赔拒赔原因");bins_是否有工作单位地址
  bins_是否有客户联系地址 <- woebin(data_是否有客户联系地址, y="险种理赔拒赔原因");bins_是否有客户联系地址
  bins_value残疾保险金 <- woebin(data_value残疾保险金, y="险种理赔拒赔原因");bins_value残疾保险金
  bins_value豁免保险金 <- woebin(data_value豁免保险金, y="险种理赔拒赔原因");bins_value豁免保险金
  bins_value门诊费用 <- woebin(data_value门诊费用, y="险种理赔拒赔原因");bins_value门诊费用
  bins_value烧烫伤保险金 <- woebin(data_value烧烫伤保险金, y="险种理赔拒赔原因");bins_value烧烫伤保险金
  bins_value身故保险金 <- woebin(data_value身故保险金, y="险种理赔拒赔原因");bins_value身故保险金
  bins_value手术费用 <- woebin(data_value手术费用, y="险种理赔拒赔原因");bins_value手术费用
  bins_value特种疾病津贴.x <- woebin(data_value特种疾病津贴.x, y="险种理赔拒赔原因");bins_value特种疾病津贴.x
  bins_value重大疾病保险金 <- woebin(data_value重大疾病保险金, y="险种理赔拒赔原因");bins_value重大疾病保险金
  bins_value重症监护津贴 <- woebin(data_value重症监护津贴, y="险种理赔拒赔原因");bins_value重症监护津贴
  bins_value住院费用 <- woebin(data_value住院费用, y="险种理赔拒赔原因");bins_value住院费用
  bins_value住院津贴 <- woebin(data_value住院津贴, y="险种理赔拒赔原因");bins_value住院津贴
  bins_value豁免 <- woebin(data_value豁免, y="险种理赔拒赔原因");bins_value豁免
  bins_value疾病末期 <- woebin(data_value疾病末期, y="险种理赔拒赔原因");bins_value疾病末期
  bins_value门诊 <- woebin(data_value门诊, y="险种理赔拒赔原因");bins_value门诊
  bins_value其它 <- woebin(data_value其它, y="险种理赔拒赔原因");bins_value其它
  bins_value烧烫伤 <- woebin(data_value烧烫伤, y="险种理赔拒赔原因");bins_value烧烫伤
  bins_value身故 <- woebin(data_value身故, y="险种理赔拒赔原因");bins_value身故
  bins_value手术 <- woebin(data_value手术, y="险种理赔拒赔原因");bins_value手术
  bins_value特种疾病津贴.y <- woebin(data_value特种疾病津贴.y, y="险种理赔拒赔原因");bins_value特种疾病津贴.y
  bins_value医疗疾病 <- woebin(data_value医疗疾病, y="险种理赔拒赔原因");bins_value医疗疾病
  bins_value重疾 <- woebin(data_value重疾, y="险种理赔拒赔原因");bins_value重疾
  bins_value重症监护 <- woebin(data_value重症监护, y="险种理赔拒赔原因");bins_value重症监护
  bins_value住院 <- woebin(data_value住院, y="险种理赔拒赔原因");bins_value住院
  bins_value残疾 <- woebin(data_value残疾, y="险种理赔拒赔原因");bins_value残疾
  bins_value101个单投保人变更 <- woebin(data_value101个单投保人变更, y="险种理赔拒赔原因");bins_value101个单投保人变更
  bins_value102客户基本资料变更 <- woebin(data_value102客户基本资料变更, y="险种理赔拒赔原因");bins_value102客户基本资料变更
  bins_value103受益人变更 <- woebin(data_value103受益人变更, y="险种理赔拒赔原因");bins_value103受益人变更
  bins_value108个单新增附约 <- woebin(data_value108个单新增附约, y="险种理赔拒赔原因");bins_value108个单新增附约
  bins_value117职业变更 <- woebin(data_value117职业变更, y="险种理赔拒赔原因");bins_value117职业变更
  bins_value125个单补充告知 <- woebin(data_value125个单补充告知, y="险种理赔拒赔原因");bins_value125个单补充告知
  bins_value161个单生日性别更正 <- woebin(data_value161个单生日性别更正, y="险种理赔拒赔原因");bins_value161个单生日性别更正
  bins_value388停效改有效 <- woebin(data_value388停效改有效, y="险种理赔拒赔原因");bins_value388停效改有效
  bins_value401个单普通复效 <- woebin(data_value401个单普通复效, y="险种理赔拒赔原因");bins_value401个单普通复效
  bins_value415免息复效 <- woebin(data_value415免息复效, y="险种理赔拒赔原因");bins_value415免息复效
}

#IV值的汇总
{
  IV_险种责任<-c(names(data_险种责任)[1],as.numeric(bins_险种责任$险种责任$total_iv[1]))
  IV_案件事故性质<-c(names(data_案件事故性质)[1],as.numeric(bins_案件事故性质$案件事故性质$total_iv[1]))
  IV_当前案件理赔申请次数<-c(names(data_当前案件理赔申请次数)[1],as.numeric(bins_当前案件理赔申请次数$当前案件理赔申请次数$total_iv[1]))
  IV_是否首次申请理赔<-c(names(data_是否首次申请理赔)[1],as.numeric(bins_是否首次申请理赔$是否首次申请理赔$total_iv[1]))
  IV_代理人是否在职<-c(names(data_代理人是否在职)[1],as.numeric(bins_代理人是否在职$代理人是否在职$total_iv[1]))
  IV_保单所属二级机构代码<-c(names(data_保单所属二级机构代码)[1],as.numeric(bins_保单所属二级机构代码$保单所属二级机构代码$total_iv[1]))
  IV_保单所属机构代码<-c(names(data_保单所属机构代码)[1],as.numeric(bins_保单所属机构代码$保单所属机构代码$total_iv[1]))
  IV_销售渠道<-c(names(data_销售渠道)[1],as.numeric(bins_销售渠道$销售渠道$total_iv[1]))
  IV_出单渠道<-c(names(data_出单渠道)[1],as.numeric(bins_出单渠道$出单渠道$total_iv[1]))
  IV_保单销售方式<-c(names(data_保单销售方式)[1],as.numeric(bins_保单销售方式$保单销售方式$total_iv[1]))
  IV_投保人与主被保人关系<-c(names(data_投保人与主被保人关系)[1],as.numeric(bins_投保人与主被保人关系$投保人与主被保人关系$total_iv[1]))
  IV_保单质押状态<-c(names(data_保单质押状态)[1],as.numeric(bins_保单质押状态$保单质押状态$total_iv[1]))
  IV_险种内部代码<-c(names(data_险种内部代码)[1],as.numeric(bins_险种内部代码$险种内部代码$total_iv[1]))
  IV_险种投保档次<-c(names(data_险种投保档次)[1],as.numeric(bins_险种投保档次$险种投保档次$total_iv[1]))
  IV_险种投保份数<-c(names(data_险种投保份数)[1],as.numeric(bins_险种投保份数$险种投保份数$total_iv[1]))
  IV_保单险种来源<-c(names(data_保单险种来源)[1],as.numeric(bins_保单险种来源$保单险种来源$total_iv[1]))
  IV_险种主附约别<-c(names(data_险种主附约别)[1],as.numeric(bins_险种主附约别$险种主附约别$total_iv[1]))
  IV_保障年期类型<-c(names(data_保障年期类型)[1],as.numeric(bins_保障年期类型$保障年期类型$total_iv[1]))
  IV_标准保障年限或年龄<-c(names(data_标准保障年限或年龄)[1],as.numeric(bins_标准保障年限或年龄$标准保障年限或年龄$total_iv[1]))
  IV_险种失效原因<-c(names(data_险种失效原因)[1],as.numeric(bins_险种失效原因$险种失效原因$total_iv[1]))
  IV_险种是否暂停<-c(names(data_险种是否暂停)[1],as.numeric(bins_险种是否暂停$险种是否暂停$total_iv[1]))
  IV_险种投保金额<-c(names(data_险种投保金额)[1],as.numeric(bins_险种投保金额$险种投保金额$total_iv[1]))
  IV_削减保额<-c(names(data_削减保额)[1],as.numeric(bins_削减保额$削减保额$total_iv[1]))
  IV_标准体当期保费<-c(names(data_标准体当期保费)[1],as.numeric(bins_标准体当期保费$标准体当期保费$total_iv[1]))
  IV_加费方法<-c(names(data_加费方法)[1],as.numeric(bins_加费方法$加费方法$total_iv[1]))
  IV_保证给付年限<-c(names(data_保证给付年限)[1],as.numeric(bins_保证给付年限$保证给付年限$total_iv[1]))
  IV_险种给付状态<-c(names(data_险种给付状态)[1],as.numeric(bins_险种给付状态$险种给付状态$total_iv[1]))
  IV_给付方式<-c(names(data_给付方式)[1],as.numeric(bins_给付方式$给付方式$total_iv[1]))
  IV_给付结束年期类型<-c(names(data_给付结束年期类型)[1],as.numeric(bins_给付结束年期类型$给付结束年期类型$total_iv[1]))
  IV_给付结束年限或年龄<-c(names(data_给付结束年限或年龄)[1],as.numeric(bins_给付结束年限或年龄$给付结束年限或年龄$total_iv[1]))
  IV_给付开始年期类型<-c(names(data_给付开始年期类型)[1],as.numeric(bins_给付开始年期类型$给付开始年期类型$total_iv[1]))
  IV_给付开始年限或年龄<-c(names(data_给付开始年限或年龄)[1],as.numeric(bins_给付开始年限或年龄$给付开始年限或年龄$total_iv[1]))
  IV_当期缴费方式<-c(names(data_当期缴费方式)[1],as.numeric(bins_当期缴费方式$当期缴费方式$total_iv[1]))
  IV_缴费年期类型<-c(names(data_缴费年期类型)[1],as.numeric(bins_缴费年期类型$缴费年期类型$total_iv[1]))
  IV_缴费年限或年龄<-c(names(data_缴费年限或年龄)[1],as.numeric(bins_缴费年限或年龄$缴费年限或年龄$total_iv[1]))
  IV_险种缴费年度<-c(names(data_险种缴费年度)[1],as.numeric(bins_险种缴费年度$险种缴费年度$total_iv[1]))
  IV_险种缴费状态<-c(names(data_险种缴费状态)[1],as.numeric(bins_险种缴费状态$险种缴费状态$total_iv[1]))
  IV_险种实际缴费期次<-c(names(data_险种实际缴费期次)[1],as.numeric(bins_险种实际缴费期次$险种实际缴费期次$total_iv[1]))
  IV_保费缴付年期<-c(names(data_保费缴付年期)[1],as.numeric(bins_保费缴付年期$保费缴付年期$total_iv[1]))
  IV_职业加费方式<-c(names(data_职业加费方式)[1],as.numeric(bins_职业加费方式$职业加费方式$total_iv[1]))
  IV_次标体开始加费类型<-c(names(data_次标体开始加费类型)[1],as.numeric(bins_次标体开始加费类型$次标体开始加费类型$total_iv[1]))
  IV_次标体加费比例<-c(names(data_次标体加费比例)[1],as.numeric(bins_次标体加费比例$次标体加费比例$total_iv[1]))
  IV_次标体加费方式<-c(names(data_次标体加费方式)[1],as.numeric(bins_次标体加费方式$次标体加费方式$total_iv[1]))
  ########IV_业务员直属部门代码<-c(names(data_业务员直属部门代码)[1],as.numeric(bins_业务员直属部门代码$业务员直属部门代码$total_iv[1]))
  IV_业务员直属机构ID<-c(names(data_业务员直属机构ID)[1],as.numeric(bins_业务员直属机构ID$业务员直属机构ID$total_iv[1]))
  IV_业务员所属二级机构ID<-c(names(data_业务员所属二级机构ID)[1],as.numeric(bins_业务员所属二级机构ID$业务员所属二级机构ID$total_iv[1]))
  IV_业务员性别<-c(names(data_业务员性别)[1],as.numeric(bins_业务员性别$业务员性别$total_iv[1]))
  IV_业务员文化程度<-c(names(data_业务员文化程度)[1],as.numeric(bins_业务员文化程度$业务员文化程度$total_iv[1]))
  IV_业务员婚姻状况<-c(names(data_业务员婚姻状况)[1],as.numeric(bins_业务员婚姻状况$业务员婚姻状况$total_iv[1]))
  IV_业务员状态<-c(names(data_业务员状态)[1],as.numeric(bins_业务员状态$业务员状态$total_iv[1]))
  IV_是否代理人自保件<-c(names(data_是否代理人自保件)[1],as.numeric(bins_是否代理人自保件$是否代理人自保件$total_iv[1]))
  IV_业务员品质职级<-c(names(data_业务员品质职级)[1],as.numeric(bins_业务员品质职级$业务员品质职级$total_iv[1]))
  IV_业务员分类<-c(names(data_业务员分类)[1],as.numeric(bins_业务员分类$业务员分类$total_iv[1]))
  IV_二次入司标志代码<-c(names(data_二次入司标志代码)[1],as.numeric(bins_二次入司标志代码$二次入司标志代码$total_iv[1]))
  IV_绿色通道授权状态代码<-c(names(data_绿色通道授权状态代码)[1],as.numeric(bins_绿色通道授权状态代码$绿色通道授权状态代码$total_iv[1]))
  IV_客户性别代码<-c(names(data_客户性别代码)[1],as.numeric(bins_客户性别代码$客户性别代码$total_iv[1]))
  IV_客户身高<-c(names(data_客户身高)[1],as.numeric(bins_客户身高$客户身高$total_iv[1]))
  IV_客户证件类型ID<-c(names(data_客户证件类型ID)[1],as.numeric(bins_客户证件类型ID$客户证件类型ID$total_iv[1]))
  IV_客户收入<-c(names(data_客户收入)[1],as.numeric(bins_客户收入$客户收入$total_iv[1]))
  IV_客户婚姻状况<-c(names(data_客户婚姻状况)[1],as.numeric(bins_客户婚姻状况$客户婚姻状况$total_iv[1]))
  IV_客户是否是一家之主<-c(names(data_客户是否是一家之主)[1],as.numeric(bins_客户是否是一家之主$客户是否是一家之主$total_iv[1]))
  IV_职业ID<-c(names(data_职业ID)[1],as.numeric(bins_职业ID$职业ID$total_iv[1]))
  IV_手机号码等级代码<-c(names(data_手机号码等级代码)[1],as.numeric(bins_手机号码等级代码$手机号码等级代码$total_iv[1]))
  ########IV_客户等级代码<-c(names(data_客户等级代码)[1],as.numeric(bins_客户等级代码$客户等级代码$total_iv[1]))
  ########IV_客户等级<-c(names(data_客户等级)[1],as.numeric(bins_客户等级$客户等级$total_iv[1]))
  IV_高端客户标识2<-c(names(data_高端客户标识2)[1],as.numeric(bins_高端客户标识2$高端客户标识2$total_iv[1]))
  IV_规保客户积分<-c(names(data_规保客户积分)[1],as.numeric(bins_规保客户积分$规保客户积分$total_iv[1]))
  IV_是否社保<-c(names(data_是否社保)[1],as.numeric(bins_是否社保$是否社保$total_iv[1]))
  IV_健康险EM值<-c(names(data_健康险EM值)[1],as.numeric(bins_健康险EM值$健康险EM值$total_iv[1]))
  IV_寿险EM值<-c(names(data_寿险EM值)[1],as.numeric(bins_寿险EM值$寿险EM值$total_iv[1]))
  IV_是否吸烟<-c(names(data_是否吸烟)[1],as.numeric(bins_是否吸烟$是否吸烟$total_iv[1]))
  IV_保单寿险保额<-c(names(data_保单寿险保额)[1],as.numeric(bins_保单寿险保额$保单寿险保额$total_iv[1]))
  IV_保单意外险保额<-c(names(data_保单意外险保额)[1],as.numeric(bins_保单意外险保额$保单意外险保额$total_iv[1]))
  IV_保单人身险保额<-c(names(data_保单人身险保额)[1],as.numeric(bins_保单人身险保额$保单人身险保额$total_iv[1]))
  IV_保单重疾险保额<-c(names(data_保单重疾险保额)[1],as.numeric(bins_保单重疾险保额$保单重疾险保额$total_iv[1]))
  IV_险种重疾保额<-c(names(data_险种重疾保额)[1],as.numeric(bins_险种重疾保额$险种重疾保额$total_iv[1]))
  IV_险种人身险保额<-c(names(data_险种人身险保额)[1],as.numeric(bins_险种人身险保额$险种人身险保额$total_iv[1]))
  IV_险种意外险保额<-c(names(data_险种意外险保额)[1],as.numeric(bins_险种意外险保额$险种意外险保额$total_iv[1]))
  IV_险种重疾险保额<-c(names(data_险种重疾险保额)[1],as.numeric(bins_险种重疾险保额$险种重疾险保额$total_iv[1]))
  IV_险种保额<-c(names(data_险种保额)[1],as.numeric(bins_险种保额$险种保额$total_iv[1]))
  IV_自动核保结论<-c(names(data_自动核保结论)[1],as.numeric(bins_自动核保结论$自动核保结论$total_iv[1]))
  IV_业务年度<-c(names(data_业务年度)[1],as.numeric(bins_业务年度$业务年度$total_iv[1]))
  IV_是否契调<-c(names(data_是否契调)[1],as.numeric(bins_是否契调$是否契调$total_iv[1]))
  ########IV_核保限制档次<-c(names(data_核保限制档次)[1],as.numeric(bins_核保限制档次$核保限制档次$total_iv[1]))
  IV_险种核保决定结论<-c(names(data_险种核保决定结论)[1],as.numeric(bins_险种核保决定结论$险种核保决定结论$total_iv[1]))
  IV_是否体检<-c(names(data_是否体检)[1],as.numeric(bins_是否体检$是否体检$total_iv[1]))
  IV_续保决定<-c(names(data_续保决定)[1],as.numeric(bins_续保决定$续保决定$total_iv[1]))
  IV_案件类型<-c(names(data_案件类型)[1],as.numeric(bins_案件类型$案件类型$total_iv[1]))
  IV_案件业务类型<-c(names(data_案件业务类型)[1],as.numeric(bins_案件业务类型$案件业务类型$total_iv[1]))
  IV_案件业务来源<-c(names(data_案件业务来源)[1],as.numeric(bins_案件业务来源$案件业务来源$total_iv[1]))
  #########IV_诊断结果<-c(names(data_诊断结果)[1],as.numeric(bins_诊断结果$诊断结果$total_iv[1]))
  IV_立案时效<-c(names(data_立案时效)[1],as.numeric(bins_立案时效$立案时效$total_iv[1]))
  IV_是否原件或复印件<-c(names(data_是否原件或复印件)[1],as.numeric(bins_是否原件或复印件$是否原件或复印件$total_iv[1]))
  IV_住院合计费用<-c(names(data_住院合计费用)[1],as.numeric(bins_住院合计费用$住院合计费用$total_iv[1]))
  IV_住院统筹支付费用<-c(names(data_住院统筹支付费用)[1],as.numeric(bins_住院统筹支付费用$住院统筹支付费用$total_iv[1]))
  IV_住院其他扣除<-c(names(data_住院其他扣除)[1],as.numeric(bins_住院其他扣除$住院其他扣除$total_iv[1]))
  IV_住院自费费用<-c(names(data_住院自费费用)[1],as.numeric(bins_住院自费费用$住院自费费用$total_iv[1]))
  IV_住院自付费用<-c(names(data_住院自付费用)[1],as.numeric(bins_住院自付费用$住院自付费用$total_iv[1]))
  IV_门诊合计费用<-c(names(data_门诊合计费用)[1],as.numeric(bins_门诊合计费用$门诊合计费用$total_iv[1]))
  IV_门诊统筹支付费用<-c(names(data_门诊统筹支付费用)[1],as.numeric(bins_门诊统筹支付费用$门诊统筹支付费用$total_iv[1]))
  IV_门诊其他扣除<-c(names(data_门诊其他扣除)[1],as.numeric(bins_门诊其他扣除$门诊其他扣除$total_iv[1]))
  IV_门诊自费费用<-c(names(data_门诊自费费用)[1],as.numeric(bins_门诊自费费用$门诊自费费用$total_iv[1]))
  IV_门诊自付费用<-c(names(data_门诊自付费用)[1],as.numeric(bins_门诊自付费用$门诊自付费用$total_iv[1]))
  IV_被保人医疗险年度保费总额<-c(names(data_被保人医疗险年度保费总额)[1],as.numeric(bins_被保人医疗险年度保费总额$被保人医疗险年度保费总额$total_iv[1]))
  IV_此保单年度保费总额<-c(names(data_此保单年度保费总额)[1],as.numeric(bins_此保单年度保费总额$此保单年度保费总额$total_iv[1]))
  IV_责任赔付金额<-c(names(data_责任赔付金额)[1],as.numeric(bins_责任赔付金额$责任赔付金额$total_iv[1]))
  IV_发票总金额<-c(names(data_发票总金额)[1],as.numeric(bins_发票总金额$发票总金额$total_iv[1]))
  IV_自费总金额<-c(names(data_自费总金额)[1],as.numeric(bins_自费总金额$自费总金额$total_iv[1]))
  IV_其它扣除<-c(names(data_其它扣除)[1],as.numeric(bins_其它扣除$其它扣除$total_iv[1]))
  IV_床位费.住院<-c(names(data_床位费.住院)[1],as.numeric(bins_床位费.住院$床位费.住院$total_iv[1]))
  IV_手术费.住院<-c(names(data_手术费.住院)[1],as.numeric(bins_手术费.住院$手术费.住院$total_iv[1]))
  IV_西药费.住院<-c(names(data_西药费.住院)[1],as.numeric(bins_西药费.住院$西药费.住院$total_iv[1]))
  IV_检查费.住院<-c(names(data_检查费.住院)[1],as.numeric(bins_检查费.住院$检查费.住院$total_iv[1]))
  IV_特殊检查费.住院<-c(names(data_特殊检查费.住院)[1],as.numeric(bins_特殊检查费.住院$特殊检查费.住院$total_iv[1]))
  IV_护理费.住院<-c(names(data_护理费.住院)[1],as.numeric(bins_护理费.住院$护理费.住院$total_iv[1]))
  IV_中成药费.住院<-c(names(data_中成药费.住院)[1],as.numeric(bins_中成药费.住院$中成药费.住院$total_iv[1]))
  IV_化验费.住院<-c(names(data_化验费.住院)[1],as.numeric(bins_化验费.住院$化验费.住院$total_iv[1]))
  IV_中草药费.住院<-c(names(data_中草药费.住院)[1],as.numeric(bins_中草药费.住院$中草药费.住院$total_iv[1]))
  IV_放射费.住院<-c(names(data_放射费.住院)[1],as.numeric(bins_放射费.住院$放射费.住院$total_iv[1]))
  IV_诊察费.住院<-c(names(data_诊察费.住院)[1],as.numeric(bins_诊察费.住院$诊察费.住院$total_iv[1]))
  IV_输血费.住院<-c(names(data_输血费.住院)[1],as.numeric(bins_输血费.住院$输血费.住院$total_iv[1]))
  IV_材料费.住院<-c(names(data_材料费.住院)[1],as.numeric(bins_材料费.住院$材料费.住院$total_iv[1]))
  IV_其他.住院<-c(names(data_其他.住院)[1],as.numeric(bins_其他.住院$其他.住院$total_iv[1]))
  IV_西药费.门诊<-c(names(data_西药费.门诊)[1],as.numeric(bins_西药费.门诊$西药费.门诊$total_iv[1]))
  IV_检查费.门诊<-c(names(data_检查费.门诊)[1],as.numeric(bins_检查费.门诊$检查费.门诊$total_iv[1]))
  IV_手术费.门诊<-c(names(data_手术费.门诊)[1],as.numeric(bins_手术费.门诊$手术费.门诊$total_iv[1]))
  IV_中成药费.住院.1<-c(names(data_中成药费.住院.1)[1],as.numeric(bins_中成药费.住院.1$中成药费.住院.1$total_iv[1]))
  IV_化验费.住院.1<-c(names(data_化验费.住院.1)[1],as.numeric(bins_化验费.住院.1$化验费.住院.1$total_iv[1]))
  IV_治疗费.门诊<-c(names(data_治疗费.门诊)[1],as.numeric(bins_治疗费.门诊$治疗费.门诊$total_iv[1]))
  IV_中草药费.门诊<-c(names(data_中草药费.门诊)[1],as.numeric(bins_中草药费.门诊$中草药费.门诊$total_iv[1]))
  IV_放射费.门诊<-c(names(data_放射费.门诊)[1],as.numeric(bins_放射费.门诊$放射费.门诊$total_iv[1]))
  IV_诊察费.门诊<-c(names(data_诊察费.门诊)[1],as.numeric(bins_诊察费.门诊$诊察费.门诊$total_iv[1]))
  IV_特殊检查费.门诊<-c(names(data_特殊检查费.门诊)[1],as.numeric(bins_特殊检查费.门诊$特殊检查费.门诊$total_iv[1]))
  IV_材料费.门诊<-c(names(data_材料费.门诊)[1],as.numeric(bins_材料费.门诊$材料费.门诊$total_iv[1]))
  IV_其他.门诊<-c(names(data_其他.门诊)[1],as.numeric(bins_其他.门诊$其他.门诊$total_iv[1]))
  ########IV_住院或门诊<-c(names(data_住院或门诊)[1],as.numeric(bins_住院或门诊$住院或门诊$total_iv[1]))
  IV_在医天数<-c(names(data_在医天数)[1],as.numeric(bins_在医天数$在医天数$total_iv[1]))
  IV_是否慢性病<-c(names(data_是否慢性病)[1],as.numeric(bins_是否慢性病$是否慢性病$total_iv[1]))
  IV_历史理赔案件数<-c(names(data_历史理赔案件数)[1],as.numeric(bins_历史理赔案件数$历史理赔案件数$total_iv[1]))
  IV_历史理赔总额<-c(names(data_历史理赔总额)[1],as.numeric(bins_历史理赔总额$历史理赔总额$total_iv[1]))
  ########IV_历史理赔险种<-c(names(data_历史理赔险种)[1],as.numeric(bins_历史理赔险种$历史理赔险种$total_iv[1]))
  IV_是否曾经被认定为欺诈<-c(names(data_是否曾经被认定为欺诈)[1],as.numeric(bins_是否曾经被认定为欺诈$是否曾经被认定为欺诈$total_iv[1]))
  IV_案件理赔申请日期减案件出险日期<-c(names(data_案件理赔申请日期减案件出险日期)[1],as.numeric(bins_案件理赔申请日期减案件出险日期$案件理赔申请日期减案件出险日期$total_iv[1]))
  IV_代理人离职时间减保单生效日期<-c(names(data_代理人离职时间减保单生效日期)[1],as.numeric(bins_代理人离职时间减保单生效日期$代理人离职时间减保单生效日期$total_iv[1]))
  IV_案件出险日期减代理人离职时间<-c(names(data_案件出险日期减代理人离职时间)[1],as.numeric(bins_案件出险日期减代理人离职时间$案件出险日期减代理人离职时间$total_iv[1]))
  IV_案件理赔申请日期减代理人离职时间<-c(names(data_案件理赔申请日期减代理人离职时间)[1],as.numeric(bins_案件理赔申请日期减代理人离职时间$案件理赔申请日期减代理人离职时间$total_iv[1]))
  IV_案件出险日期减险种生效时间<-c(names(data_案件出险日期减险种生效时间)[1],as.numeric(bins_案件出险日期减险种生效时间$案件出险日期减险种生效时间$total_iv[1]))
  IV_案件理赔申请日期减险种生效时间<-c(names(data_案件理赔申请日期减险种生效时间)[1],as.numeric(bins_案件理赔申请日期减险种生效时间$案件理赔申请日期减险种生效时间$total_iv[1]))
  IV_业务员年龄<-c(names(data_业务员年龄)[1],as.numeric(bins_业务员年龄$业务员年龄$total_iv[1]))
  IV_业务员入司年限<-c(names(data_业务员入司年限)[1],as.numeric(bins_业务员入司年限$业务员入司年限$total_iv[1]))
  IV_客户年龄<-c(names(data_客户年龄)[1],as.numeric(bins_客户年龄$客户年龄$total_iv[1]))
  IV_是否有工作单位名称<-c(names(data_是否有工作单位名称)[1],as.numeric(bins_是否有工作单位名称$是否有工作单位名称$total_iv[1]))
  IV_是否有工作单位地址<-c(names(data_是否有工作单位地址)[1],as.numeric(bins_是否有工作单位地址$是否有工作单位地址$total_iv[1]))
  IV_是否有客户联系地址<-c(names(data_是否有客户联系地址)[1],as.numeric(bins_是否有客户联系地址$是否有客户联系地址$total_iv[1]))
  IV_value残疾保险金<-c(names(data_value残疾保险金)[1],as.numeric(bins_value残疾保险金$value残疾保险金$total_iv[1]))
  IV_value豁免保险金<-c(names(data_value豁免保险金)[1],as.numeric(bins_value豁免保险金$value豁免保险金$total_iv[1]))
  IV_value门诊费用<-c(names(data_value门诊费用)[1],as.numeric(bins_value门诊费用$value门诊费用$total_iv[1]))
  IV_value烧烫伤保险金<-c(names(data_value烧烫伤保险金)[1],as.numeric(bins_value烧烫伤保险金$value烧烫伤保险金$total_iv[1]))
  IV_value身故保险金<-c(names(data_value身故保险金)[1],as.numeric(bins_value身故保险金$value身故保险金$total_iv[1]))
  IV_value手术费用<-c(names(data_value手术费用)[1],as.numeric(bins_value手术费用$value手术费用$total_iv[1]))
  IV_value特种疾病津贴.x<-c(names(data_value特种疾病津贴.x)[1],as.numeric(bins_value特种疾病津贴.x$value特种疾病津贴.x$total_iv[1]))
  IV_value重大疾病保险金<-c(names(data_value重大疾病保险金)[1],as.numeric(bins_value重大疾病保险金$value重大疾病保险金$total_iv[1]))
  IV_value重症监护津贴<-c(names(data_value重症监护津贴)[1],as.numeric(bins_value重症监护津贴$value重症监护津贴$total_iv[1]))
  IV_value住院费用<-c(names(data_value住院费用)[1],as.numeric(bins_value住院费用$value住院费用$total_iv[1]))
  IV_value住院津贴<-c(names(data_value住院津贴)[1],as.numeric(bins_value住院津贴$value住院津贴$total_iv[1]))
  IV_value豁免<-c(names(data_value豁免)[1],as.numeric(bins_value豁免$value豁免$total_iv[1]))
  IV_value疾病末期<-c(names(data_value疾病末期)[1],as.numeric(bins_value疾病末期$value疾病末期$total_iv[1]))
  IV_value门诊<-c(names(data_value门诊)[1],as.numeric(bins_value门诊$value门诊$total_iv[1]))
  IV_value其它<-c(names(data_value其它)[1],as.numeric(bins_value其它$value其它$total_iv[1]))
  IV_value烧烫伤<-c(names(data_value烧烫伤)[1],as.numeric(bins_value烧烫伤$value烧烫伤$total_iv[1]))
  IV_value身故<-c(names(data_value身故)[1],as.numeric(bins_value身故$value身故$total_iv[1]))
  IV_value手术<-c(names(data_value手术)[1],as.numeric(bins_value手术$value手术$total_iv[1]))
  IV_value特种疾病津贴.y<-c(names(data_value特种疾病津贴.y)[1],as.numeric(bins_value特种疾病津贴.y$value特种疾病津贴.y$total_iv[1]))
  IV_value医疗疾病<-c(names(data_value医疗疾病)[1],as.numeric(bins_value医疗疾病$value医疗疾病$total_iv[1]))
  IV_value重疾<-c(names(data_value重疾)[1],as.numeric(bins_value重疾$value重疾$total_iv[1]))
  IV_value重症监护<-c(names(data_value重症监护)[1],as.numeric(bins_value重症监护$value重症监护$total_iv[1]))
  IV_value住院<-c(names(data_value住院)[1],as.numeric(bins_value住院$value住院$total_iv[1]))
  IV_value残疾<-c(names(data_value残疾)[1],as.numeric(bins_value残疾$value残疾$total_iv[1]))
  IV_value101个单投保人变更<-c(names(data_value101个单投保人变更)[1],as.numeric(bins_value101个单投保人变更$value101个单投保人变更$total_iv[1]))
  IV_value102客户基本资料变更<-c(names(data_value102客户基本资料变更)[1],as.numeric(bins_value102客户基本资料变更$value102客户基本资料变更$total_iv[1]))
  IV_value103受益人变更<-c(names(data_value103受益人变更)[1],as.numeric(bins_value103受益人变更$value103受益人变更$total_iv[1]))
  IV_value108个单新增附约<-c(names(data_value108个单新增附约)[1],as.numeric(bins_value108个单新增附约$value108个单新增附约$total_iv[1]))
  IV_value117职业变更<-c(names(data_value117职业变更)[1],as.numeric(bins_value117职业变更$value117职业变更$total_iv[1]))
  IV_value125个单补充告知<-c(names(data_value125个单补充告知)[1],as.numeric(bins_value125个单补充告知$value125个单补充告知$total_iv[1]))
  IV_value161个单生日性别更正<-c(names(data_value161个单生日性别更正)[1],as.numeric(bins_value161个单生日性别更正$value161个单生日性别更正$total_iv[1]))
  IV_value388停效改有效<-c(names(data_value388停效改有效)[1],as.numeric(bins_value388停效改有效$value388停效改有效$total_iv[1]))
  IV_value401个单普通复效<-c(names(data_value401个单普通复效)[1],as.numeric(bins_value401个单普通复效$value401个单普通复效$total_iv[1]))
  IV_value415免息复效<-c(names(data_value415免息复效)[1],as.numeric(bins_value415免息复效$value415免息复效$total_iv[1]))
  
  IV_TOTAL<-rbind(
    IV_险种责任,
    IV_案件事故性质,
    IV_当前案件理赔申请次数,
    IV_是否首次申请理赔,
    IV_代理人是否在职,
    IV_保单所属二级机构代码,
    IV_保单所属机构代码,
    IV_销售渠道,
    IV_出单渠道,
    IV_保单销售方式,
    IV_投保人与主被保人关系,
    IV_保单质押状态,
    IV_险种内部代码,
    IV_险种投保档次,
    IV_险种投保份数,
    IV_保单险种来源,
    IV_险种主附约别,
    IV_保障年期类型,
    IV_标准保障年限或年龄,
    IV_险种失效原因,
    IV_险种是否暂停,
    IV_险种投保金额,
    IV_削减保额,
    IV_标准体当期保费,
    IV_加费方法,
    IV_保证给付年限,
    IV_险种给付状态,
    IV_给付方式,
    IV_给付结束年期类型,
    IV_给付结束年限或年龄,
    IV_给付开始年期类型,
    IV_给付开始年限或年龄,
    IV_当期缴费方式,
    IV_缴费年期类型,
    IV_缴费年限或年龄,
    IV_险种缴费年度,
    IV_险种缴费状态,
    IV_险种实际缴费期次,
    IV_保费缴付年期,
    IV_职业加费方式,
    IV_次标体开始加费类型,
    IV_次标体加费比例,
    IV_次标体加费方式,
    ######## IV_业务员直属部门代码,
    IV_业务员直属机构ID,
    IV_业务员所属二级机构ID,
    IV_业务员性别,
    IV_业务员文化程度,
    IV_业务员婚姻状况,
    IV_业务员状态,
    IV_是否代理人自保件,
    IV_业务员品质职级,
    IV_业务员分类,
    IV_二次入司标志代码,
    IV_绿色通道授权状态代码,
    IV_客户性别代码,
    IV_客户身高,
    IV_客户证件类型ID,
    IV_客户收入,
    IV_客户婚姻状况,
    IV_客户是否是一家之主,
    IV_职业ID,
    IV_手机号码等级代码,
    ######## IV_客户等级代码,
    ######## IV_客户等级,
    IV_高端客户标识2,
    IV_规保客户积分,
    IV_是否社保,
    IV_健康险EM值,
    IV_寿险EM值,
    IV_是否吸烟,
    IV_保单寿险保额,
    IV_保单意外险保额,
    IV_保单人身险保额,
    IV_保单重疾险保额,
    IV_险种重疾保额,
    IV_险种人身险保额,
    IV_险种意外险保额,
    IV_险种重疾险保额,
    IV_险种保额,
    IV_自动核保结论,
    IV_业务年度,
    IV_是否契调,
    ######## IV_核保限制档次,
    IV_险种核保决定结论,
    IV_是否体检,
    IV_续保决定,
    IV_案件类型,
    IV_案件业务类型,
    IV_案件业务来源,
    ######## IV_诊断结果,
    IV_立案时效,
    IV_是否原件或复印件,
    IV_住院合计费用,
    IV_住院统筹支付费用,
    IV_住院其他扣除,
    IV_住院自费费用,
    IV_住院自付费用,
    IV_门诊合计费用,
    IV_门诊统筹支付费用,
    IV_门诊其他扣除,
    IV_门诊自费费用,
    IV_门诊自付费用,
    IV_被保人医疗险年度保费总额,
    IV_此保单年度保费总额,
    IV_责任赔付金额,
    IV_发票总金额,
    IV_自费总金额,
    IV_其它扣除,
    IV_床位费.住院,
    IV_手术费.住院,
    IV_西药费.住院,
    IV_检查费.住院,
    IV_特殊检查费.住院,
    IV_护理费.住院,
    IV_中成药费.住院,
    IV_化验费.住院,
    IV_中草药费.住院,
    IV_放射费.住院,
    IV_诊察费.住院,
    IV_输血费.住院,
    IV_材料费.住院,
    IV_其他.住院,
    IV_西药费.门诊,
    IV_检查费.门诊,
    IV_手术费.门诊,
    IV_中成药费.住院.1,
    IV_化验费.住院.1,
    IV_治疗费.门诊,
    IV_中草药费.门诊,
    IV_放射费.门诊,
    IV_诊察费.门诊,
    IV_特殊检查费.门诊,
    IV_材料费.门诊,
    IV_其他.门诊,
    ###### IV_住院或门诊,
    IV_在医天数,
    IV_是否慢性病,
    IV_历史理赔案件数,
    IV_历史理赔总额,
    ###### IV_历史理赔险种,
    IV_是否曾经被认定为欺诈,
    IV_案件理赔申请日期减案件出险日期,
    IV_代理人离职时间减保单生效日期,
    IV_案件出险日期减代理人离职时间,
    IV_案件理赔申请日期减代理人离职时间,
    IV_案件出险日期减险种生效时间,
    IV_案件理赔申请日期减险种生效时间,
    IV_业务员年龄,
    IV_业务员入司年限,
    IV_客户年龄,
    IV_是否有工作单位名称,
    IV_是否有工作单位地址,
    IV_是否有客户联系地址,
    IV_value残疾保险金,
    IV_value豁免保险金,
    IV_value门诊费用,
    IV_value烧烫伤保险金,
    IV_value身故保险金,
    IV_value手术费用,
    IV_value特种疾病津贴.x,
    IV_value重大疾病保险金,
    IV_value重症监护津贴,
    IV_value住院费用,
    IV_value住院津贴,
    IV_value豁免,
    IV_value疾病末期,
    IV_value门诊,
    IV_value其它,
    IV_value烧烫伤,
    IV_value身故,
    IV_value手术,
    IV_value特种疾病津贴.y,
    IV_value医疗疾病,
    IV_value重疾,
    IV_value重症监护,
    IV_value住院,
    IV_value残疾,
    IV_value101个单投保人变更,
    IV_value102客户基本资料变更,
    IV_value103受益人变更,
    IV_value108个单新增附约,
    IV_value117职业变更,
    IV_value125个单补充告知,
    IV_value161个单生日性别更正,
    IV_value388停效改有效,
    IV_value401个单普通复效,
    IV_value415免息复效
  )
  
  View(IV_TOTAL)
}

######################################################################
######################## 3. 经过IV选定的变量 #########################
######################################################################

############## 3.1 IV值为判断依据的变量的初选和再选 ############

#IV选择后的变量（初选）--step2_finalData01
{
  step2_finalData01<-data[c(
    "险种缴费状态",#6.607874356
    "发票总金额",#3.968296044
    "自费总金额",#3.857179199
    "其它扣除",#3.837251136
    "险种缴费年度",#2.089689578
    "险种内部代码",#1.863825389
    "案件出险日期减险种生效时间",#1.812042154
    "业务年度",#1.767372408
    "险种实际缴费期次",#1.753736222
    "案件理赔申请日期减险种生效时间",#1.541364738
    "责任赔付金额",#1.255640293
    "案件出险日期减代理人离职时间",#0.747445966
    "案件理赔申请日期减代理人离职时间",#0.697661404
    "出单渠道",#0.632108329
    "险种投保金额",#0.629743233
    "险种保额",#0.628457507
    "险种责任",#0.522175692
    "标准保障年限或年龄",#0.484381479
    "当前案件理赔申请次数",#0.483442252
    "保障年期类型",#0.457990732
    "历史理赔案件数",#0.404854525
    "案件理赔申请日期减案件出险日期",#0.371651353
    "历史理赔总额",#0.352341548
    "立案时效",#0.332397099
    "业务员入司年限",#0.323512018
    "业务员直属机构ID",#0.312692642
    "险种重疾险保额",#0.265565274
    "案件事故性质",#0.263798726
    "代理人离职时间减保单生效日期",#0.227970399
    "险种主附约别",#0.211980269
    "检查费.住院",#0.211292294
    "缴费年限或年龄",#0.209853249
    "规保客户积分",#0.208748966
    "被保人医疗险年度保费总额",#0.189412073
    "value101个单投保人变更",#0.183050743
    "value102客户基本资料变更",#0.183050743
    "value103受益人变更",#0.183050743
    "value108个单新增附约",#0.183050743
    "value117职业变更",#0.183050743
    "value125个单补充告知",#0.183050743
    "value161个单生日性别更正",#0.183050743
    "value388停效改有效",#0.183050743
    "value401个单普通复效",#0.183050743
    "value415免息复效",#0.183050743
    "化验费.住院",#0.175151468
    "保单意外险保额",#0.159832232
    "住院自付费用",#0.151364058
    "客户年龄",#0.148165607
    "住院合计费用",#0.147196675
    "value门诊费用",#0.141926236
    "住院统筹支付费用",#0.141689361
    "业务员所属二级机构ID",#0.126097297
    "保单所属二级机构代码",#0.125553384
    "value门诊",#0.124547341
    "客户收入",#0.123574944
    "西药费.住院",#0.107389178
    "门诊自付费用",#0.106916694
    "门诊合计费用",#0.104901679
    "门诊自费费用",#0.10394099
    "西药费.门诊",#0.101836974
    "检查费.门诊",#0.101836974
    "手术费.门诊",#0.101836974
    "中成药费.住院.1",#0.101836974
    "化验费.住院.1",#0.101836974
    "治疗费.门诊",#0.101836974
    "中草药费.门诊",#0.101836974
    "放射费.门诊",#0.101836974
    "诊察费.门诊",#0.101836974
    "特殊检查费.门诊",#0.101836974
    "材料费.门诊",#0.101836974
    "其他.门诊",#0.101836974
    "此保单年度保费总额",#0.096991722
    "投保人与主被保人关系",#0.094720919
    "是否慢性病",#0.087983319
    "保单人身险保额",#0.083927111
    "门诊统筹支付费用",#0.070379323
    "住院自费费用",#0.069228374
    "门诊其他扣除",#0.068280076
    "保单重疾险保额",#0.048824591
    "是否社保",#0.048066129
    "保单寿险保额",#0.0418334
    
    "险种理赔拒赔原因"
  )]
  str(step2_finalData01)
}

#初选后的binning、IV等的再考察，包括个别变量binning的调整
#标注的为有问题的，无问题的无标注
{
  
  bins_险种缴费状态 <- woebin(data_险种缴费状态, y="险种理赔拒赔原因");bins_险种缴费状态
  
  bins_发票总金额 <- woebin(data_发票总金额, y="险种理赔拒赔原因");bins_发票总金额
  
  bins_自费总金额 <- woebin(data_自费总金额, y="险种理赔拒赔原因");bins_自费总金额
  
  #建议删除；missing较多且都为0，非空的未分段
  bins_其它扣除 <- woebin(data_其它扣除, y="险种理赔拒赔原因");bins_其它扣除
  
  bins_险种缴费年度 <- woebin(data_险种缴费年度, y="险种理赔拒赔原因");bins_险种缴费年度
  
  bins_险种内部代码 <- woebin(data_险种内部代码, y="险种理赔拒赔原因");bins_险种内部代码
  
  bins_案件出险日期减险种生效时间 <- woebin(data_案件出险日期减险种生效时间, y="险种理赔拒赔原因");bins_案件出险日期减险种生效时间
  
  bins_业务年度 <- woebin(data_业务年度, y="险种理赔拒赔原因");bins_业务年度
  
  bins_险种实际缴费期次 <- woebin(data_险种实际缴费期次, y="险种理赔拒赔原因");bins_险种实际缴费期次
  
  bins_案件理赔申请日期减险种生效时间 <- woebin(data_案件理赔申请日期减险种生效时间, y="险种理赔拒赔原因");bins_案件理赔申请日期减险种生效时间
  
  bins_责任赔付金额 <- woebin(data_责任赔付金额, y="险种理赔拒赔原因");bins_责任赔付金额
  
  bins_案件出险日期减代理人离职时间 <- woebin(data_案件出险日期减代理人离职时间, y="险种理赔拒赔原因");bins_案件出险日期减代理人离职时间
  
  bins_案件理赔申请日期减代理人离职时间 <- woebin(data_案件理赔申请日期减代理人离职时间, y="险种理赔拒赔原因");bins_案件理赔申请日期减代理人离职时间
  
  bins_出单渠道 <- woebin(data_出单渠道, y="险种理赔拒赔原因");bins_出单渠道
  
  #建议删除；
  bins_险种投保金额 <- woebin(data_险种投保金额, y="险种理赔拒赔原因");bins_险种投保金额
  
  bins_险种保额 <- woebin(data_险种保额, y="险种理赔拒赔原因");bins_险种保额
  
  bins_险种责任 <- woebin(data_险种责任, y="险种理赔拒赔原因");bins_险种责任
  
  bins_标准保障年限或年龄 <- woebin(data_标准保障年限或年龄, y="险种理赔拒赔原因");bins_标准保障年限或年龄
  
  bins_当前案件理赔申请次数 <- woebin(data_当前案件理赔申请次数, y="险种理赔拒赔原因");bins_当前案件理赔申请次数
  
  bins_保障年期类型 <- woebin(data_保障年期类型, y="险种理赔拒赔原因");bins_保障年期类型
  
  bins_历史理赔案件数 <- woebin(data_历史理赔案件数, y="险种理赔拒赔原因");bins_历史理赔案件数
  
  bins_案件理赔申请日期减案件出险日期 <- woebin(data_案件理赔申请日期减案件出险日期, y="险种理赔拒赔原因");bins_案件理赔申请日期减案件出险日期
  
  bins_历史理赔总额 <- woebin(data_历史理赔总额, y="险种理赔拒赔原因");bins_历史理赔总额
  
  # 只有一个missing 
  bins_立案时效 <- woebin(data_立案时效, y="险种理赔拒赔原因");bins_立案时效
  
  bins_业务员入司年限 <- woebin(data_业务员入司年限, y="险种理赔拒赔原因");bins_业务员入司年限
  
  bins_业务员直属机构ID <- woebin(data_业务员直属机构ID, y="险种理赔拒赔原因");bins_业务员直属机构ID
  
  bins_险种重疾险保额 <- woebin(data_险种重疾险保额, y="险种理赔拒赔原因");bins_险种重疾险保额
  
  #24个missing
  bins_案件事故性质 <- woebin(data_案件事故性质, y="险种理赔拒赔原因");bins_案件事故性质
  
  bins_代理人离职时间减保单生效日期 <- woebin(data_代理人离职时间减保单生效日期, y="险种理赔拒赔原因");bins_代理人离职时间减保单生效日期
  
  bins_险种主附约别 <- woebin(data_险种主附约别, y="险种理赔拒赔原因");bins_险种主附约别
  
  bins_检查费.住院 <- woebin(data_检查费.住院, y="险种理赔拒赔原因");bins_检查费.住院
  
  #4个missing
  bins_缴费年限或年龄 <- woebin(data_缴费年限或年龄, y="险种理赔拒赔原因");bins_缴费年限或年龄
  
  bins_规保客户积分 <- woebin(data_规保客户积分, y="险种理赔拒赔原因");bins_规保客户积分
  
  bins_被保人医疗险年度保费总额 <- woebin(data_被保人医疗险年度保费总额, y="险种理赔拒赔原因");bins_被保人医疗险年度保费总额
  
  #调整BUNNING
  bins_value101个单投保人变更 <- woebin(data_value101个单投保人变更, y="险种理赔拒赔原因");bins_value101个单投保人变更
  breaks_adj_value101个单投保人变更 <- woebin_adj(data_value101个单投保人变更, "险种理赔拒赔原因", bins_value101个单投保人变更)
  bins_adj_value101个单投保人变更 <- woebin(data_value101个单投保人变更, "险种理赔拒赔原因", breaks_list=breaks_adj_value101个单投保人变更, print_step=0)
  
  #调整BUNNING  
  bins_value102客户基本资料变更 <- woebin(data_value102客户基本资料变更, y="险种理赔拒赔原因");bins_value102客户基本资料变更
  breaks_adj_value102客户基本资料变更 <- woebin_adj(data_value102客户基本资料变更, "险种理赔拒赔原因", bins_value102客户基本资料变更)
  bins_adj_value102客户基本资料变更 <- woebin(data_value102客户基本资料变更, "险种理赔拒赔原因", breaks_list=breaks_adj_value102客户基本资料变更, print_step=0)
  
  #调整BUNNING
  bins_value103受益人变更 <- woebin(data_value103受益人变更, y="险种理赔拒赔原因");bins_value103受益人变更
  breaks_adj_value103受益人变更 <- woebin_adj(data_value103受益人变更, "险种理赔拒赔原因", bins_value103受益人变更)
  bins_adj_value103受益人变更 <- woebin(data_value103受益人变更, "险种理赔拒赔原因", breaks_list=breaks_adj_value103受益人变更, print_step=0)
  
  #调整BUNNING
  bins_value108个单新增附约 <- woebin(data_value108个单新增附约, y="险种理赔拒赔原因");bins_value108个单新增附约
  breaks_adj_value108个单新增附约 <- woebin_adj(data_value108个单新增附约, "险种理赔拒赔原因", bins_value108个单新增附约)
  bins_adj_value108个单新增附约 <- woebin(data_value108个单新增附约, "险种理赔拒赔原因", breaks_list=breaks_adj_value108个单新增附约, print_step=0)
  
  #调整BUNNING
  bins_value117职业变更 <- woebin(data_value117职业变更, y="险种理赔拒赔原因");bins_value117职业变更
  breaks_adj_value117职业变更 <- woebin_adj(data_value117职业变更, "险种理赔拒赔原因", bins_value117职业变更)
  bins_adj_value117职业变更  <- woebin(data_value117职业变更, "险种理赔拒赔原因", breaks_list=breaks_adj_value117职业变更, print_step=0)
  
  #调整BUNNING
  bins_value125个单补充告知 <- woebin(data_value125个单补充告知, y="险种理赔拒赔原因");bins_value125个单补充告知
  breaks_adj_value125个单补充告知 <- woebin_adj(data_value125个单补充告知, "险种理赔拒赔原因", bins_value125个单补充告知)
  bins_adj_value125个单补充告知 <- woebin(data_value125个单补充告知, "险种理赔拒赔原因", breaks_list=breaks_adj_value125个单补充告知, print_step=0)
  
  #调整BUNNING
  bins_value161个单生日性别更正 <- woebin(data_value161个单生日性别更正, y="险种理赔拒赔原因");bins_value161个单生日性别更正
  breaks_adj_value161个单生日性别更正 <- woebin_adj(data_value161个单生日性别更正, "险种理赔拒赔原因", bins_value161个单生日性别更正)
  bins_adj_value161个单生日性别更正 <- woebin(data_value161个单生日性别更正, "险种理赔拒赔原因", breaks_list=breaks_adj_value161个单生日性别更正, print_step=0)
  
  #调整BUNNING
  bins_value388停效改有效 <- woebin(data_value388停效改有效, y="险种理赔拒赔原因");bins_value388停效改有效
  breaks_adj_value388停效改有效 <- woebin_adj(data_value388停效改有效, "险种理赔拒赔原因", bins_value388停效改有效)
  bins_adj_value388停效改有效 <- woebin(data_value388停效改有效, "险种理赔拒赔原因", breaks_list=breaks_adj_value388停效改有效, print_step=0)
  
  #调整BUNNING
  bins_value401个单普通复效 <- woebin(data_value401个单普通复效, y="险种理赔拒赔原因");bins_value401个单普通复效
  breaks_adj_value401个单普通复效 <- woebin_adj(data_value401个单普通复效, "险种理赔拒赔原因", bins_value401个单普通复效)
  bins_adj_value401个单普通复效 <- woebin(data_value401个单普通复效, "险种理赔拒赔原因", breaks_list=breaks_adj_value401个单普通复效, print_step=0)
  
  #调整BUNNING
  bins_value415免息复效 <- woebin(data_value415免息复效, y="险种理赔拒赔原因");bins_value415免息复效
  breaks_adj_value415免息复效 <- woebin_adj(data_value415免息复效, "险种理赔拒赔原因", bins_value415免息复效)
  bins_adj_value415免息复效 <- woebin(data_value415免息复效, "险种理赔拒赔原因", breaks_list=breaks_adj_value415免息复效, print_step=0)
  
  bins_化验费.住院 <- woebin(data_化验费.住院, y="险种理赔拒赔原因");bins_化验费.住院
  
  bins_保单意外险保额 <- woebin(data_保单意外险保额, y="险种理赔拒赔原因");bins_保单意外险保额
  
  bins_住院自付费用 <- woebin(data_住院自付费用, y="险种理赔拒赔原因");bins_住院自付费用
  
  bins_客户年龄 <- woebin(data_客户年龄, y="险种理赔拒赔原因");bins_客户年龄
  
  bins_住院合计费用 <- woebin(data_住院合计费用, y="险种理赔拒赔原因");bins_住院合计费用
  
  #62个missing
  bins_value门诊费用 <- woebin(data_value门诊费用, y="险种理赔拒赔原因");bins_value门诊费用
  
  bins_住院统筹支付费用 <- woebin(data_住院统筹支付费用, y="险种理赔拒赔原因");bins_住院统筹支付费用
  
  bins_业务员所属二级机构ID <- woebin(data_业务员所属二级机构ID, y="险种理赔拒赔原因");bins_业务员所属二级机构ID
  
  #和业务员所属二级机构ID重复，删除。
  #bins_保单所属二级机构代码 <- woebin(data_保单所属二级机构代码, y="险种理赔拒赔原因");bins_保单所属二级机构代码
  
  #6个missing
  bins_value门诊 <- woebin(data_value门诊, y="险种理赔拒赔原因");bins_value门诊
  
  bins_客户收入 <- woebin(data_客户收入, y="险种理赔拒赔原因");bins_客户收入
  
  bins_西药费.住院 <- woebin(data_西药费.住院, y="险种理赔拒赔原因");bins_西药费.住院
  
  #missing过多,建议删除
  bins_门诊自付费用 <- woebin(data_门诊自付费用, y="险种理赔拒赔原因");bins_门诊自付费用
  
  #missing过多,建议删除
  bins_门诊合计费用 <- woebin(data_门诊合计费用, y="险种理赔拒赔原因");bins_门诊合计费用
  
  #missing过多,建议删除
  bins_门诊自费费用 <- woebin(data_门诊自费费用, y="险种理赔拒赔原因");bins_门诊自费费用
  
  #missing过多，且无binning,建议删除
  bins_西药费.门诊 <- woebin(data_西药费.门诊, y="险种理赔拒赔原因");bins_西药费.门诊
  
  #missing过多，且无binning,建议删除
  bins_检查费.门诊 <- woebin(data_检查费.门诊, y="险种理赔拒赔原因");bins_检查费.门诊
  
  #missing过多，且无binning,建议删除
  bins_手术费.门诊 <- woebin(data_手术费.门诊, y="险种理赔拒赔原因");bins_手术费.门诊
  
  #missing过多，且无binning,建议删除
  bins_中成药费.住院.1 <- woebin(data_中成药费.住院.1, y="险种理赔拒赔原因");bins_中成药费.住院.1
  
  #missing过多，且无binning,建议删除
  bins_化验费.住院.1 <- woebin(data_化验费.住院.1, y="险种理赔拒赔原因");bins_化验费.住院.1
  
  #missing过多，且无binning,建议删除
  bins_治疗费.门诊 <- woebin(data_治疗费.门诊, y="险种理赔拒赔原因");bins_治疗费.门诊
  
  #missing过多，且无binning,建议删除
  bins_中草药费.门诊 <- woebin(data_中草药费.门诊, y="险种理赔拒赔原因");bins_中草药费.门诊
  
  #missing过多，且无binning,建议删除
  bins_放射费.门诊 <- woebin(data_放射费.门诊, y="险种理赔拒赔原因");bins_放射费.门诊
  
  #missing过多，且无binning,建议删除
  bins_诊察费.门诊 <- woebin(data_诊察费.门诊, y="险种理赔拒赔原因");bins_诊察费.门诊
  
  #missing过多，且无binning,建议删除
  bins_特殊检查费.门诊 <- woebin(data_特殊检查费.门诊, y="险种理赔拒赔原因");bins_特殊检查费.门诊
  
  #missing过多，且无binning,建议删除
  bins_材料费.门诊 <- woebin(data_材料费.门诊, y="险种理赔拒赔原因");bins_材料费.门诊
  
  #missing过多，且无binning,建议删除
  bins_其他.门诊 <- woebin(data_其他.门诊, y="险种理赔拒赔原因");bins_其他.门诊
  
  bins_此保单年度保费总额 <- woebin(data_此保单年度保费总额, y="险种理赔拒赔原因");bins_此保单年度保费总额
  
  bins_投保人与主被保人关系 <- woebin(data_投保人与主被保人关系, y="险种理赔拒赔原因");bins_投保人与主被保人关系
  
  bins_是否慢性病 <- woebin(data_是否慢性病, y="险种理赔拒赔原因");bins_是否慢性病
  
  bins_保单人身险保额 <- woebin(data_保单人身险保额, y="险种理赔拒赔原因");bins_保单人身险保额
  
  #missing过多，且无binning,建议删除
  bins_门诊统筹支付费用 <- woebin(data_门诊统筹支付费用, y="险种理赔拒赔原因");bins_门诊统筹支付费用
  
  bins_住院自费费用 <- woebin(data_住院自费费用, y="险种理赔拒赔原因");bins_住院自费费用
  
  bins_门诊其他扣除 <- woebin(data_门诊其他扣除, y="险种理赔拒赔原因");bins_门诊其他扣除
  
  #missing过多，且无binning,建议删除
  bins_保单重疾险保额 <- woebin(data_保单重疾险保额, y="险种理赔拒赔原因");bins_保单重疾险保额
  
  bins_是否社保 <- woebin(data_是否社保, y="险种理赔拒赔原因");bins_是否社保
  
  bins_保单寿险保额 <- woebin(data_保单寿险保额, y="险种理赔拒赔原因");bins_保单寿险保额
}

#IV选择后的变量（选）--step2_finalData02,57 variables
{
  step2_finalData02<-data[c(
    #"险种缴费状态",#6.607874356,missing values are all "good"
    #"发票总金额",#3.968296044,missing values are all "good"
    # "自费总金额",#3.857179199,missing values are all "good"
    # "其它扣除",#3.837251136,missing values are all "good"
    "险种缴费年度",#2.089689578
    "险种内部代码",#1.863825389
    "案件出险日期减险种生效时间",#1.812042154
    "业务年度",#1.767372408
    "险种实际缴费期次",#1.753736222
    "案件理赔申请日期减险种生效时间",#1.541364738
    "责任赔付金额",#1.255640293
    "案件出险日期减代理人离职时间",#0.747445966
    "案件理赔申请日期减代理人离职时间",#0.697661404
    "出单渠道",#0.632108329
    #"险种投保金额",#0.629743233,same with 险种保额,建议删除
    "险种保额",#0.628457507
    "险种责任",#0.522175692
    "标准保障年限或年龄",#0.484381479
    "当前案件理赔申请次数",#0.483442252
    "保障年期类型",#0.457990732
    "历史理赔案件数",#0.404854525
    "案件理赔申请日期减案件出险日期",#0.371651353
    "历史理赔总额",#0.352341548
    "立案时效",#0.332397099
    "业务员入司年限",#0.323512018
    "业务员直属机构ID",#0.312692642
    "险种重疾险保额",#0.265565274
    "案件事故性质",#0.263798726
    "代理人离职时间减保单生效日期",#0.227970399
    "险种主附约别",#0.211980269
    "检查费.住院",#0.211292294
    "缴费年限或年龄",#0.209853249
    "规保客户积分",#0.208748966
    "被保人医疗险年度保费总额",#0.189412073
    "value101个单投保人变更",#0.183050743
    "value102客户基本资料变更",#0.183050743
    "value103受益人变更",#0.183050743
    "value108个单新增附约",#0.183050743
    "value117职业变更",#0.183050743
    "value125个单补充告知",#0.183050743
    "value161个单生日性别更正",#0.183050743
    "value388停效改有效",#0.183050743
    #"value401个单普通复效",#0.183050743
    #"value415免息复效",#0.183050743
    "化验费.住院",#0.175151468
    "保单意外险保额",#0.159832232#missing多，业务含义大
    "住院自付费用",#0.151364058
    "客户年龄",#0.148165607
    "住院合计费用",#0.147196675
    "value门诊费用",#0.141926236
    "住院统筹支付费用",#0.141689361
    "业务员所属二级机构ID",#0.126097297
    #  "保单所属二级机构代码",#0.125553384
    "value门诊",#0.124547341
    "客户收入",#0.123574944
    "西药费.住院",#0.107389178
    #  "门诊自付费用",#0.106916694,missing过多
    # "门诊合计费用",#0.104901679,missing过多
    #  "门诊自费费用",#0.10394099,missing过多
    #  "西药费.门诊",#0.101836974,missing过多，且无binning,建议删除 
    #  "检查费.门诊",#0.101836974,missing过多，且无binning,建议删除
    #  "手术费.门诊",#0.101836974,missing过多，且无binning,建议删除
    #  "中成药费.住院.1",#0.101836974,missing过多，且无binning,建议删除
    #  "化验费.住院.1",#0.101836974,missing过多，且无binning,建议删除
    # "治疗费.门诊",#0.101836974,missing过多，且无binning,建议删除
    # "中草药费.门诊",#0.101836974,missing过多，且无binning,建议删除
    # "放射费.门诊",#0.101836974,missing过多，且无binning,建议删除
    # "诊察费.门诊",#0.101836974,missing过多，且无binning,建议删除
    # "特殊检查费.门诊",#0.101836974,missing过多，且无binning,建议删除
    # "材料费.门诊",#0.101836974,missing过多，且无binning,建议删除
    # "其他.门诊",#0.101836974,missing过多，且无binning,建议删除
    "此保单年度保费总额",#0.096991722
    "投保人与主被保人关系",#0.094720919
    "是否慢性病",#0.087983319
    "保单人身险保额",#0.083927111
    # "门诊统筹支付费用",#0.070379323,missing过多，且无binning,建议删除
    "住院自费费用",#0.069228374
    # "门诊其他扣除",#0.068280076,missing过多，且无binning,建议删除
    # "保单重疾险保额",#0.048824591
    "是否社保",#0.048066129
    "保单寿险保额",#0.0418334
    
    "险种理赔拒赔原因"
  )]
  names(step2_finalData02)
}

################ 3.2 原始训练数据替换成WOE值 ###################

train <- as.data.frame(step2_finalData02);head(train)
#原始数据集转换为对应的woe值woebin_ply-57 variables
{
  train_woe_险种缴费年度<-woebin_ply(as.data.frame(train[c("险种缴费年度","险种理赔拒赔原因")]),bins_险种缴费年度,print_step=0)
  train_woe_险种内部代码<-woebin_ply(as.data.frame(train[c("险种内部代码","险种内部代码")]),bins_险种内部代码,print_step=0)
  train_woe_案件出险日期减险种生效时间<-woebin_ply(as.data.frame(train[c("案件出险日期减险种生效时间","案件出险日期减险种生效时间")]),bins_案件出险日期减险种生效时间,print_step=0)
  train_woe_业务年度<-woebin_ply(as.data.frame(train[c("业务年度","业务年度")]),bins_业务年度,print_step=0)
  train_woe_险种实际缴费期次<-woebin_ply(as.data.frame(train[c("险种实际缴费期次","险种实际缴费期次")]),bins_险种实际缴费期次,print_step=0)
  train_woe_案件理赔申请日期减险种生效时间<-woebin_ply(as.data.frame(train[c("案件理赔申请日期减险种生效时间","案件理赔申请日期减险种生效时间")]),bins_案件理赔申请日期减险种生效时间,print_step=0)
  train_woe_责任赔付金额<-woebin_ply(as.data.frame(train[c("责任赔付金额","责任赔付金额")]),bins_责任赔付金额,print_step=0)
  train_woe_案件出险日期减代理人离职时间<-woebin_ply(as.data.frame(train[c("案件出险日期减代理人离职时间","案件出险日期减代理人离职时间")]),bins_案件出险日期减代理人离职时间,print_step=0)
  train_woe_案件理赔申请日期减代理人离职时间<-woebin_ply(as.data.frame(train[c("案件理赔申请日期减代理人离职时间","案件理赔申请日期减代理人离职时间")]),bins_案件理赔申请日期减代理人离职时间,print_step=0)
  train_woe_出单渠道<-woebin_ply(as.data.frame(train[c("出单渠道","出单渠道")]),bins_出单渠道,print_step=0)
  train_woe_险种保额<-woebin_ply(as.data.frame(train[c("险种保额","险种保额")]),bins_险种保额,print_step=0)
  train_woe_险种责任<-woebin_ply(as.data.frame(train[c("险种责任","险种责任")]),bins_险种责任,print_step=0)
  train_woe_标准保障年限或年龄<-woebin_ply(as.data.frame(train[c("标准保障年限或年龄","标准保障年限或年龄")]),bins_标准保障年限或年龄,print_step=0)
  train_woe_当前案件理赔申请次数<-woebin_ply(as.data.frame(train[c("当前案件理赔申请次数","当前案件理赔申请次数")]),bins_当前案件理赔申请次数,print_step=0)
  train_woe_保障年期类型<-woebin_ply(as.data.frame(train[c("保障年期类型","保障年期类型")]),bins_保障年期类型,print_step=0)
  train_woe_历史理赔案件数<-woebin_ply(as.data.frame(train[c("历史理赔案件数","历史理赔案件数")]),bins_历史理赔案件数,print_step=0)
  train_woe_案件理赔申请日期减案件出险日期<-woebin_ply(as.data.frame(train[c("案件理赔申请日期减案件出险日期","案件理赔申请日期减案件出险日期")]),bins_案件理赔申请日期减案件出险日期,print_step=0)
  train_woe_历史理赔总额<-woebin_ply(as.data.frame(train[c("历史理赔总额","历史理赔总额")]),bins_历史理赔总额,print_step=0)
  train_woe_立案时效<-woebin_ply(as.data.frame(train[c("立案时效","立案时效")]),bins_立案时效,print_step=0)
  train_woe_业务员入司年限<-woebin_ply(as.data.frame(train[c("业务员入司年限","业务员入司年限")]),bins_业务员入司年限,print_step=0)
  train_woe_业务员直属机构ID<-woebin_ply(as.data.frame(train[c("业务员直属机构ID","业务员直属机构ID")]),bins_业务员直属机构ID,print_step=0)
  train_woe_险种重疾险保额<-woebin_ply(as.data.frame(train[c("险种重疾险保额","险种重疾险保额")]),bins_险种重疾险保额,print_step=0)
  train_woe_案件事故性质<-woebin_ply(as.data.frame(train[c("案件事故性质","案件事故性质")]),bins_案件事故性质,print_step=0)
  train_woe_代理人离职时间减保单生效日期<-woebin_ply(as.data.frame(train[c("代理人离职时间减保单生效日期","代理人离职时间减保单生效日期")]),bins_代理人离职时间减保单生效日期,print_step=0)
  train_woe_险种主附约别<-woebin_ply(as.data.frame(train[c("险种主附约别","险种主附约别")]),bins_险种主附约别,print_step=0)
  train_woe_检查费.住院<-woebin_ply(as.data.frame(train[c("检查费.住院","检查费.住院")]),bins_检查费.住院,print_step=0)
  train_woe_缴费年限或年龄<-woebin_ply(as.data.frame(train[c("缴费年限或年龄","缴费年限或年龄")]),bins_缴费年限或年龄,print_step=0)
  train_woe_规保客户积分<-woebin_ply(as.data.frame(train[c("规保客户积分","规保客户积分")]),bins_规保客户积分,print_step=0)
  train_woe_被保人医疗险年度保费总额<-woebin_ply(as.data.frame(train[c("被保人医疗险年度保费总额","被保人医疗险年度保费总额")]),bins_被保人医疗险年度保费总额,print_step=0)
  
  train_woe_value101个单投保人变更<-woebin_ply(as.data.frame(train[c("value101个单投保人变更","value101个单投保人变更")]),bins_adj_value101个单投保人变更,print_step=0)
  
  train_woe_value102客户基本资料变更<-woebin_ply(as.data.frame(train[c("value102客户基本资料变更","value102客户基本资料变更")]),bins_adj_value102客户基本资料变更,print_step=0)
  
  train_woe_value103受益人变更<-woebin_ply(as.data.frame(train[c("value103受益人变更","value103受益人变更")]),bins_adj_value103受益人变更,print_step=0)
  
  train_woe_value108个单新增附约<-woebin_ply(as.data.frame(train[c("value108个单新增附约","value108个单新增附约")]),bins_adj_value108个单新增附约,print_step=0)
  
  train_woe_value117职业变更<-woebin_ply(as.data.frame(train[c("value117职业变更","value117职业变更")]),bins_adj_value117职业变更,print_step=0)
  
  train_woe_value125个单补充告知<-woebin_ply(as.data.frame(train[c("value125个单补充告知","value125个单补充告知")]),bins_adj_value125个单补充告知,print_step=0)
  
  train_woe_value161个单生日性别更正<-woebin_ply(as.data.frame(train[c("value161个单生日性别更正","value161个单生日性别更正")]),bins_adj_value161个单生日性别更正,print_step=0)
  
  #train_woe_value388停效改有效<-woebin_ply(as.data.frame(train[c("value388停效改有效","value388停效改有效")]),bins_adj_value388停效改有效,print_step=0)
  
  #train_woe_value401个单普通复效<-woebin_ply(as.data.frame(train[c("value401个单普通复效","value401个单普通复效")]),bins_adj_value401个单普通复效,print_step=0)
  
  train_woe_value415免息复效<-woebin_ply(as.data.frame(train[c("value415免息复效","value415免息复效")]),bins_adj_value415免息复效,print_step=0)
  train_woe_化验费.住院<-woebin_ply(as.data.frame(train[c("化验费.住院","化验费.住院")]),bins_化验费.住院,print_step=0)
  train_woe_保单意外险保额<-woebin_ply(as.data.frame(train[c("保单意外险保额","保单意外险保额")]),bins_保单意外险保额,print_step=0)
  train_woe_住院自付费用<-woebin_ply(as.data.frame(train[c("住院自付费用","住院自付费用")]),bins_住院自付费用,print_step=0)
  train_woe_客户年龄<-woebin_ply(as.data.frame(train[c("客户年龄","客户年龄")]),bins_客户年龄,print_step=0)
  train_woe_住院合计费用<-woebin_ply(as.data.frame(train[c("住院合计费用","住院合计费用")]),bins_住院合计费用,print_step=0)
  train_woe_value门诊费用<-woebin_ply(as.data.frame(train[c("value门诊费用","value门诊费用")]),bins_value门诊费用,print_step=0)
  train_woe_住院统筹支付费用<-woebin_ply(as.data.frame(train[c("住院统筹支付费用","住院统筹支付费用")]),bins_住院统筹支付费用,print_step=0)
  train_woe_业务员所属二级机构ID<-woebin_ply(as.data.frame(train[c("业务员所属二级机构ID","业务员所属二级机构ID")]),bins_业务员所属二级机构ID,print_step=0)
  train_woe_value门诊<-woebin_ply(as.data.frame(train[c("value门诊","value门诊")]),bins_value门诊,print_step=0)
  train_woe_客户收入<-woebin_ply(as.data.frame(train[c("客户收入","客户收入")]),bins_客户收入,print_step=0)
  train_woe_西药费.住院<-woebin_ply(as.data.frame(train[c("西药费.住院","西药费.住院")]),bins_西药费.住院,print_step=0)
  train_woe_此保单年度保费总额<-woebin_ply(as.data.frame(train[c("此保单年度保费总额","此保单年度保费总额")]),bins_此保单年度保费总额,print_step=0)
  train_woe_投保人与主被保人关系<-woebin_ply(as.data.frame(train[c("投保人与主被保人关系","投保人与主被保人关系")]),bins_投保人与主被保人关系,print_step=0)
  train_woe_是否慢性病<-woebin_ply(as.data.frame(train[c("是否慢性病","是否慢性病")]),bins_是否慢性病,print_step=0)
  train_woe_保单人身险保额<-woebin_ply(as.data.frame(train[c("保单人身险保额","保单人身险保额")]),bins_保单人身险保额,print_step=0)
  train_woe_住院自费费用<-woebin_ply(as.data.frame(train[c("住院自费费用","住院自费费用")]),bins_住院自费费用,print_step=0)
  train_woe_是否社保<-woebin_ply(as.data.frame(train[c("是否社保","是否社保")]),bins_是否社保,print_step=0)
  train_woe_保单寿险保额<-woebin_ply(as.data.frame(train[c("保单寿险保额","保单寿险保额")]),bins_保单寿险保额,print_step=0)
}  

#train_woe_all，WOE的汇总
{
  train_woe_all<-cbind(
    #train_woe_险种缴费状态[,2],#6.607874356,missing values are all good
    #train_woe_发票总金额[,2],#3.968296044,missing values are all good
    # train_woe_自费总金额[,2],#3.857179199,missing values are all good
    # train_woe_其它扣除[,2],#3.837251136,missing values are all good
    train_woe_险种缴费年度,#2.089689578 
    train_woe_险种内部代码[,2],#1.863825389
    train_woe_案件出险日期减险种生效时间[,2],#1.812042154
    train_woe_业务年度[,2],#1.767372408
    train_woe_险种实际缴费期次[,2],#1.753736222
    train_woe_案件理赔申请日期减险种生效时间[,2],#1.541364738
    train_woe_责任赔付金额[,2],#1.255640293
    train_woe_案件出险日期减代理人离职时间[,2],#0.747445966
    train_woe_案件理赔申请日期减代理人离职时间[,2],#0.697661404
    train_woe_出单渠道[,2],#0.632108329
    #train_woe_险种投保金额[,2],#0.629743233,same with 险种保额,建议删除
    train_woe_险种保额[,2],#0.628457507
    train_woe_险种责任[,2],#0.522175692
    train_woe_标准保障年限或年龄[,2],#0.484381479
    train_woe_当前案件理赔申请次数[,2],#0.483442252
    train_woe_保障年期类型[,2],#0.457990732
    train_woe_历史理赔案件数[,2],#0.404854525
    train_woe_案件理赔申请日期减案件出险日期[,2],#0.371651353
    train_woe_历史理赔总额[,2],#0.352341548
    train_woe_立案时效[,2],#0.332397099
    train_woe_业务员入司年限[,2],#0.323512018
    train_woe_业务员直属机构ID[,2],#0.312692642
    train_woe_险种重疾险保额[,2],#0.265565274
    train_woe_案件事故性质[,2],#0.263798726
    train_woe_代理人离职时间减保单生效日期[,2],#0.227970399
    train_woe_险种主附约别[,2],#0.211980269
    train_woe_检查费.住院[,2],#0.211292294
    train_woe_缴费年限或年龄[,2],#0.209853249
    train_woe_规保客户积分[,2],#0.208748966
    train_woe_被保人医疗险年度保费总额[,2],#0.189412073
    train_woe_value101个单投保人变更[,2],#0.183050743
    train_woe_value102客户基本资料变更[,2],#0.183050743
    train_woe_value103受益人变更[,2],#0.183050743
    train_woe_value108个单新增附约[,2],#0.183050743
    train_woe_value117职业变更[,2],#0.183050743
    train_woe_value125个单补充告知[,2],#0.183050743
    train_woe_value161个单生日性别更正[,2],#0.183050743
    train_woe_value388停效改有效[,2],#0.183050743
    # train_woe_value401个单普通复效[,2],#0.183050743
    # train_woe_value415免息复效[,2],#0.183050743
    train_woe_化验费.住院[,2],#0.175151468
    train_woe_保单意外险保额[,2],#0.159832232#missing多，业务含义大
    train_woe_住院自付费用[,2],#0.151364058
    train_woe_客户年龄[,2],#0.148165607
    train_woe_住院合计费用[,2],#0.147196675
    train_woe_value门诊费用[,2],#0.141926236
    train_woe_住院统筹支付费用[,2],#0.141689361
    train_woe_业务员所属二级机构ID[,2],#0.126097297
    #  train_woe_保单所属二级机构代码[,2],#0.125553384
    train_woe_value门诊[,2],#0.124547341
    train_woe_客户收入[,2],#0.123574944
    train_woe_西药费.住院[,2],#0.107389178
    #  train_woe_门诊自付费用[,2],#0.106916694,missing过多
    # train_woe_门诊合计费用[,2],#0.104901679,missing过多
    #  train_woe_门诊自费费用[,2],#0.10394099,missing过多
    #  train_woe_西药费.门诊[,2],#0.101836974,missing过多，且无binning,建议删除 
    #  train_woe_检查费.门诊[,2],#0.101836974,missing过多，且无binning,建议删除
    #  train_woe_手术费.门诊[,2],#0.101836974,missing过多，且无binning,建议删除
    #  train_woe_中成药费.住院.1[,2],#0.101836974,missing过多，且无binning,建议删除
    #  train_woe_化验费.住院.1[,2],#0.101836974,missing过多，且无binning,建议删除
    # train_woe_治疗费.门诊[,2],#0.101836974,missing过多，且无binning,建议删除
    # train_woe_中草药费.门诊[,2],#0.101836974,missing过多，且无binning,建议删除
    # train_woe_放射费.门诊[,2],#0.101836974,missing过多，且无binning,建议删除
    # train_woe_诊察费.门诊[,2],#0.101836974,missing过多，且无binning,建议删除
    # train_woe_特殊检查费.门诊[,2],#0.101836974,missing过多，且无binning,建议删除
    # train_woe_材料费.门诊[,2],#0.101836974,missing过多，且无binning,建议删除
    # train_woe_其他.门诊[,2],#0.101836974,missing过多，且无binning,建议删除
    train_woe_此保单年度保费总额[,2],#0.096991722
    train_woe_投保人与主被保人关系[,2],#0.094720919
    train_woe_是否慢性病[,2],#0.087983319
    train_woe_保单人身险保额[,2],#0.083927111
    # train_woe_门诊统筹支付费用[,2],#0.070379323,missing过多，且无binning,建议删除
    train_woe_住院自费费用[,2],#0.069228374
    # train_woe_门诊其他扣除[,2],#0.068280076,missing过多，且无binning,建议删除
    # train_woe_保单重疾险保额[,2],#0.048824591
    train_woe_是否社保[,2],#0.048066129
    train_woe_保单寿险保额[,2]#0.0418334
  )
  
  head(train_woe_all)
  names(train_woe_all)
  nrow(train_woe_all)
}

######################################################################
####################### 4. 逻辑回归模型的建立 ########################
######################################################################

################### 4.1 逻辑回归模型的建立 #####################
#使用glm(binomial)和lasso
{
  m1 <- glm( 险种理赔拒赔原因 ~ ., family = "binomial", data = train_woe_all)  
  summary(m1)
  
  # 基于AIC筛选变量
  # 也可通过lasso实现变量筛选，具体参考上面提到的幻灯片
  # m2_step <- step(m1, direction="both", trace = FALSE)
  # m2 <- eval(m2_step$call)
  # summary(m2)
}

################### 4.2 逻辑回归模型的评估 #####################
# 预测违约概率-使用training数据，及perf_eva函数
{
  train_pred <- predict(m1, train_woe_all, type='response')
  summary(train_pred);head(train_pred)
  perf_eva(train$险种理赔拒赔原因, train_pred, title = "train")
}

#计算混淆矩阵及相应指标-技术指标
{
  predType <-c()
  for (i in 1:length(train_pred))
  {
    if (train_pred[i]<=0.2)
    {
      predType[i]<-0
    }
    if (train_pred[i]>0.2)
    {
      predType[i]<-1
    }
  }
  table(predType)
  
  table(train_woe_all$险种理赔拒赔原因)
  
  # ks & roc plot
  # type可设定返回的模型评估指标，包括"ks", "lift", "roc", "pr"
  confusionMatrix<-as.matrix(table(train_woe_all$险种理赔拒赔原因,predType))
  confusionMatrix
}

#计算混淆矩阵及相应指标-业务指标
{
  
}

######################################################################
########################## 5. 模型的测试 #############################
######################################################################

#################### 5.1 测试数据的转换 #######################
#test数据的输入-test_20180701_20180815
{
  test_20180701_20180715 <- read.csv("D:/R/ORIGINAL_DATA/20180701-20180815 transformed/20180701-20180715transformed.csv", header=TRUE, sep=",", 
                                     na.strings = "", blank.lines.skip=F)
  #summary(test_20180701_20180715$险种理赔拒赔原因)
  test<-test_20180701_20180715[,5:193]
  names(test)
}

#test变量的筛选-test_finalData
{
  test_finalData<-test[c(
    #"险种缴费状态",#6.607874356,missing values are all "good"
    #"发票总金额",#3.968296044,missing values are all "good"
    # "自费总金额",#3.857179199,missing values are all "good"
    # "其它扣除",#3.837251136,missing values are all "good"
    "险种缴费年度",#2.089689578
    "险种内部代码",#1.863825389
    "案件出险日期减险种生效时间",#1.812042154
    "业务年度",#1.767372408
    "险种实际缴费期次",#1.753736222
    "案件理赔申请日期减险种生效时间",#1.541364738
    "责任赔付金额",#1.255640293
    "案件出险日期减代理人离职时间",#0.747445966
    "案件理赔申请日期减代理人离职时间",#0.697661404
    "出单渠道",#0.632108329
    #"险种投保金额",#0.629743233,same with 险种保额,建议删除
    "险种保额",#0.628457507
    "险种责任",#0.522175692
    "标准保障年限或年龄",#0.484381479
    "当前案件理赔申请次数",#0.483442252
    "保障年期类型",#0.457990732
    "历史理赔案件数",#0.404854525
    "案件理赔申请日期减案件出险日期",#0.371651353
    "历史理赔总额",#0.352341548
    "立案时效",#0.332397099
    "业务员入司年限",#0.323512018
    "业务员直属机构ID",#0.312692642
    "险种重疾险保额",#0.265565274
    "案件事故性质",#0.263798726
    "代理人离职时间减保单生效日期",#0.227970399
    "险种主附约别",#0.211980269
    "检查费.住院",#0.211292294
    "缴费年限或年龄",#0.209853249
    "规保客户积分",#0.208748966
    "被保人医疗险年度保费总额",#0.189412073
    "value101个单投保人变更",#0.183050743
    "value102客户基本资料变更",#0.183050743
    "value103受益人变更",#0.183050743
    "value108个单新增附约",#0.183050743
    "value117职业变更",#0.183050743
    "value125个单补充告知",#0.183050743
    "value161个单生日性别更正",#0.183050743
    "value388停效改有效",#0.183050743
    #"value401个单普通复效",#0.183050743
    # "value415免息复效",#0.183050743
    "化验费.住院",#0.175151468
    "保单意外险保额",#0.159832232#missing多，业务含义大
    "住院自付费用",#0.151364058
    "客户年龄",#0.148165607
    "住院合计费用",#0.147196675
    "value门诊费用",#0.141926236
    "住院统筹支付费用",#0.141689361
    "业务员所属二级机构ID",#0.126097297
    #  "保单所属二级机构代码",#0.125553384
    "value门诊",#0.124547341
    "客户收入",#0.123574944
    "西药费.住院",#0.107389178
    #  "门诊自付费用",#0.106916694,missing过多
    # "门诊合计费用",#0.104901679,missing过多
    #  "门诊自费费用",#0.10394099,missing过多
    #  "西药费.门诊",#0.101836974,missing过多，且无binning,建议删除 
    #  "检查费.门诊",#0.101836974,missing过多，且无binning,建议删除
    #  "手术费.门诊",#0.101836974,missing过多，且无binning,建议删除
    #  "中成药费.住院.1",#0.101836974,missing过多，且无binning,建议删除
    #  "化验费.住院.1",#0.101836974,missing过多，且无binning,建议删除
    # "治疗费.门诊",#0.101836974,missing过多，且无binning,建议删除
    # "中草药费.门诊",#0.101836974,missing过多，且无binning,建议删除
    # "放射费.门诊",#0.101836974,missing过多，且无binning,建议删除
    # "诊察费.门诊",#0.101836974,missing过多，且无binning,建议删除
    # "特殊检查费.门诊",#0.101836974,missing过多，且无binning,建议删除
    # "材料费.门诊",#0.101836974,missing过多，且无binning,建议删除
    # "其他.门诊",#0.101836974,missing过多，且无binning,建议删除
    "此保单年度保费总额",#0.096991722
    "投保人与主被保人关系",#0.094720919
    "是否慢性病",#0.087983319
    "保单人身险保额",#0.083927111
    # "门诊统筹支付费用",#0.070379323,missing过多，且无binning,建议删除
    "住院自费费用",#0.069228374
    # "门诊其他扣除",#0.068280076,missing过多，且无binning,建议删除
    # "保单重疾险保额",#0.048824591
    "是否社保",#0.048066129
    "保单寿险保额",
    "保单寿险保额"#0.0418334
  )]
  
  names(test_finalData)
  head(test_finalData)
  nrow(test_finalData)
}

################ 5.2 原始测试数据替换成WOE值 ##################
#原始数据集转换为对应的woe值woebin_ply
{
  test_woe_险种缴费年度<-woebin_ply(as.data.frame(test[c("险种缴费年度","险种缴费年度")]),bins_险种缴费年度,print_step=0)
  test_woe_险种内部代码<-woebin_ply(as.data.frame(test[c("险种内部代码","险种内部代码")]),bins_险种内部代码,print_step=0)
  test_woe_案件出险日期减险种生效时间<-woebin_ply(as.data.frame(test[c("案件出险日期减险种生效时间","案件出险日期减险种生效时间")]),bins_案件出险日期减险种生效时间,print_step=0)
  test_woe_业务年度<-woebin_ply(as.data.frame(test[c("业务年度","业务年度")]),bins_业务年度,print_step=0)
  test_woe_险种实际缴费期次<-woebin_ply(as.data.frame(test[c("险种实际缴费期次","险种实际缴费期次")]),bins_险种实际缴费期次,print_step=0)
  test_woe_案件理赔申请日期减险种生效时间<-woebin_ply(as.data.frame(test[c("案件理赔申请日期减险种生效时间","案件理赔申请日期减险种生效时间")]),bins_案件理赔申请日期减险种生效时间,print_step=0)
  test_woe_责任赔付金额<-woebin_ply(as.data.frame(test[c("责任赔付金额","责任赔付金额")]),bins_责任赔付金额,print_step=0)
  test_woe_案件出险日期减代理人离职时间<-woebin_ply(as.data.frame(test[c("案件出险日期减代理人离职时间","案件出险日期减代理人离职时间")]),bins_案件出险日期减代理人离职时间,print_step=0)
  test_woe_案件理赔申请日期减代理人离职时间<-woebin_ply(as.data.frame(test[c("案件理赔申请日期减代理人离职时间","案件理赔申请日期减代理人离职时间")]),bins_案件理赔申请日期减代理人离职时间,print_step=0)
  test_woe_出单渠道<-woebin_ply(as.data.frame(test[c("出单渠道","出单渠道")]),bins_出单渠道,print_step=0)
  test_woe_险种保额<-woebin_ply(as.data.frame(test[c("险种保额","险种保额")]),bins_险种保额,print_step=0)
  test_woe_险种责任<-woebin_ply(as.data.frame(test[c("险种责任","险种责任")]),bins_险种责任,print_step=0)
  test_woe_标准保障年限或年龄<-woebin_ply(as.data.frame(test[c("标准保障年限或年龄","标准保障年限或年龄")]),bins_标准保障年限或年龄,print_step=0)
  test_woe_当前案件理赔申请次数<-woebin_ply(as.data.frame(test[c("当前案件理赔申请次数","当前案件理赔申请次数")]),bins_当前案件理赔申请次数,print_step=0)
  test_woe_保障年期类型<-woebin_ply(as.data.frame(test[c("保障年期类型","保障年期类型")]),bins_保障年期类型,print_step=0)
  test_woe_历史理赔案件数<-woebin_ply(as.data.frame(test[c("历史理赔案件数","历史理赔案件数")]),bins_历史理赔案件数,print_step=0)
  test_woe_案件理赔申请日期减案件出险日期<-woebin_ply(as.data.frame(test[c("案件理赔申请日期减案件出险日期","案件理赔申请日期减案件出险日期")]),bins_案件理赔申请日期减案件出险日期,print_step=0)
  test_woe_历史理赔总额<-woebin_ply(as.data.frame(test[c("历史理赔总额","历史理赔总额")]),bins_历史理赔总额,print_step=0)
  test_woe_立案时效<-woebin_ply(as.data.frame(test[c("立案时效","立案时效")]),bins_立案时效,print_step=0)
  test_woe_业务员入司年限<-woebin_ply(as.data.frame(test[c("业务员入司年限","业务员入司年限")]),bins_业务员入司年限,print_step=0)
  test_woe_业务员直属机构ID<-woebin_ply(as.data.frame(test[c("业务员直属机构ID","业务员直属机构ID")]),bins_业务员直属机构ID,print_step=0)
  test_woe_险种重疾险保额<-woebin_ply(as.data.frame(test[c("险种重疾险保额","险种重疾险保额")]),bins_险种重疾险保额,print_step=0)
  test_woe_案件事故性质<-woebin_ply(as.data.frame(test[c("案件事故性质","案件事故性质")]),bins_案件事故性质,print_step=0)
  test_woe_代理人离职时间减保单生效日期<-woebin_ply(as.data.frame(test[c("代理人离职时间减保单生效日期","代理人离职时间减保单生效日期")]),bins_代理人离职时间减保单生效日期,print_step=0)
  test_woe_险种主附约别<-woebin_ply(as.data.frame(test[c("险种主附约别","险种主附约别")]),bins_险种主附约别,print_step=0)
  test_woe_检查费.住院<-woebin_ply(as.data.frame(test[c("检查费.住院","检查费.住院")]),bins_检查费.住院,print_step=0)
  test_woe_缴费年限或年龄<-woebin_ply(as.data.frame(test[c("缴费年限或年龄","缴费年限或年龄")]),bins_缴费年限或年龄,print_step=0)
  test_woe_规保客户积分<-woebin_ply(as.data.frame(test[c("规保客户积分","规保客户积分")]),bins_规保客户积分,print_step=0)
  test_woe_被保人医疗险年度保费总额<-woebin_ply(as.data.frame(test[c("被保人医疗险年度保费总额","被保人医疗险年度保费总额")]),bins_被保人医疗险年度保费总额,print_step=0)
  
  test_woe_value101个单投保人变更<-woebin_ply(as.data.frame(test[c("value101个单投保人变更","value101个单投保人变更")]),bins_adj_value101个单投保人变更,print_step=0)
  
  test_woe_value102客户基本资料变更<-woebin_ply(as.data.frame(test[c("value102客户基本资料变更","value102客户基本资料变更")]),bins_adj_value102客户基本资料变更,print_step=0)
  
  test_woe_value103受益人变更<-woebin_ply(as.data.frame(test[c("value103受益人变更","value103受益人变更")]),bins_adj_value103受益人变更,print_step=0)
  
  test_woe_value108个单新增附约<-woebin_ply(as.data.frame(test[c("value108个单新增附约","value108个单新增附约")]),bins_adj_value108个单新增附约,print_step=0)
  
  test_woe_value117职业变更<-woebin_ply(as.data.frame(test[c("value117职业变更","value117职业变更")]),bins_adj_value117职业变更,print_step=0)
  
  test_woe_value125个单补充告知<-woebin_ply(as.data.frame(test[c("value125个单补充告知","value125个单补充告知")]),bins_adj_value125个单补充告知,print_step=0)
  
  test_woe_value161个单生日性别更正<-woebin_ply(as.data.frame(test[c("value161个单生日性别更正","value161个单生日性别更正")]),bins_adj_value161个单生日性别更正,print_step=0)
  
  #test_woe_value388停效改有效<-woebin_ply(as.data.frame(test[c("value388停效改有效","value388停效改有效")]),bins_adj_value388停效改有效,print_step=0)
  
  #test_woe_value401个单普通复效<-woebin_ply(as.data.frame(test[c("value401个单普通复效","value401个单普通复效")]),bins_adj_value401个单普通复效,print_step=0)
  
  test_woe_value415免息复效<-woebin_ply(as.data.frame(test[c("value415免息复效","value415免息复效")]),bins_adj_value415免息复效,print_step=0)
  test_woe_化验费.住院<-woebin_ply(as.data.frame(test[c("化验费.住院","化验费.住院")]),bins_化验费.住院,print_step=0)
  test_woe_保单意外险保额<-woebin_ply(as.data.frame(test[c("保单意外险保额","保单意外险保额")]),bins_保单意外险保额,print_step=0)
  test_woe_住院自付费用<-woebin_ply(as.data.frame(test[c("住院自付费用","住院自付费用")]),bins_住院自付费用,print_step=0)
  test_woe_客户年龄<-woebin_ply(as.data.frame(test[c("客户年龄","客户年龄")]),bins_客户年龄,print_step=0)
  test_woe_住院合计费用<-woebin_ply(as.data.frame(test[c("住院合计费用","住院合计费用")]),bins_住院合计费用,print_step=0)
  test_woe_value门诊费用<-woebin_ply(as.data.frame(test[c("value门诊费用","value门诊费用")]),bins_value门诊费用,print_step=0)
  test_woe_住院统筹支付费用<-woebin_ply(as.data.frame(test[c("住院统筹支付费用","住院统筹支付费用")]),bins_住院统筹支付费用,print_step=0)
  test_woe_业务员所属二级机构ID<-woebin_ply(as.data.frame(test[c("业务员所属二级机构ID","业务员所属二级机构ID")]),bins_业务员所属二级机构ID,print_step=0)
  test_woe_value门诊<-woebin_ply(as.data.frame(test[c("value门诊","value门诊")]),bins_value门诊,print_step=0)
  test_woe_客户收入<-woebin_ply(as.data.frame(test[c("客户收入","客户收入")]),bins_客户收入,print_step=0)
  test_woe_西药费.住院<-woebin_ply(as.data.frame(test[c("西药费.住院","西药费.住院")]),bins_西药费.住院,print_step=0)
  test_woe_此保单年度保费总额<-woebin_ply(as.data.frame(test[c("此保单年度保费总额","此保单年度保费总额")]),bins_此保单年度保费总额,print_step=0)
  test_woe_投保人与主被保人关系<-woebin_ply(as.data.frame(test[c("投保人与主被保人关系","投保人与主被保人关系")]),bins_投保人与主被保人关系,print_step=0)
  test_woe_是否慢性病<-woebin_ply(as.data.frame(test[c("是否慢性病","是否慢性病")]),bins_是否慢性病,print_step=0)
  test_woe_保单人身险保额<-woebin_ply(as.data.frame(test[c("保单人身险保额","保单人身险保额")]),bins_保单人身险保额,print_step=0)
  test_woe_住院自费费用<-woebin_ply(as.data.frame(test[c("住院自费费用","住院自费费用")]),bins_住院自费费用,print_step=0)
  test_woe_是否社保<-woebin_ply(as.data.frame(test[c("是否社保","是否社保")]),bins_是否社保,print_step=0)
  test_woe_保单寿险保额<-woebin_ply(as.data.frame(test[c("保单寿险保额","保单寿险保额")]),bins_保单寿险保额,print_step=0)
}

#test_woe_all
{
  test_woe_all<-cbind(
    #test_woe_险种缴费状态[,2],#6.607874356,missing values are all good
    #test_woe_发票总金额[,2],#3.968296044,missing values are all good
    # test_woe_自费总金额[,2],#3.857179199,missing values are all good
    # test_woe_其它扣除[,2],#3.837251136,missing values are all good
    test_woe_险种缴费年度[,2],#2.089689578 
    test_woe_险种内部代码[,2],#1.863825389
    test_woe_案件出险日期减险种生效时间[,2],#1.812042154
    test_woe_业务年度[,2],#1.767372408
    test_woe_险种实际缴费期次[,2],#1.753736222
    test_woe_案件理赔申请日期减险种生效时间[,2],#1.541364738
    test_woe_责任赔付金额[,2],#1.255640293
    test_woe_案件出险日期减代理人离职时间[,2],#0.747445966
    test_woe_案件理赔申请日期减代理人离职时间[,2],#0.697661404
    test_woe_出单渠道[,2],#0.632108329
    #test_woe_险种投保金额[,2],#0.629743233,same with 险种保额,建议删除
    test_woe_险种保额[,2],#0.628457507
    test_woe_险种责任[,2],#0.522175692
    test_woe_标准保障年限或年龄[,2],#0.484381479
    test_woe_当前案件理赔申请次数[,2],#0.483442252
    test_woe_保障年期类型[,2],#0.457990732
    test_woe_历史理赔案件数[,2],#0.404854525
    test_woe_案件理赔申请日期减案件出险日期[,2],#0.371651353
    test_woe_历史理赔总额[,2],#0.352341548
    test_woe_立案时效[,2],#0.332397099
    test_woe_业务员入司年限[,2],#0.323512018
    test_woe_业务员直属机构ID[,2],#0.312692642
    test_woe_险种重疾险保额[,2],#0.265565274
    test_woe_案件事故性质[,2],#0.263798726
    test_woe_代理人离职时间减保单生效日期[,2],#0.227970399
    test_woe_险种主附约别[,2],#0.211980269
    test_woe_检查费.住院[,2],#0.211292294
    test_woe_缴费年限或年龄[,2],#0.209853249
    test_woe_规保客户积分[,2],#0.208748966
    test_woe_被保人医疗险年度保费总额[,2],#0.189412073
    test_woe_value101个单投保人变更[,2],#0.183050743
    test_woe_value102客户基本资料变更[,2],#0.183050743
    test_woe_value103受益人变更[,2],#0.183050743
    test_woe_value108个单新增附约[,2],#0.183050743
    test_woe_value117职业变更[,2],#0.183050743
    test_woe_value125个单补充告知[,2],#0.183050743
    test_woe_value161个单生日性别更正[,2],#0.183050743
    test_woe_value388停效改有效[,2],#0.183050743
    # test_woe_value401个单普通复效[,2],#0.183050743
    #test_woe_value415免息复效[,2],#0.183050743
    test_woe_化验费.住院[,2],#0.175151468
    test_woe_保单意外险保额[,2],#0.159832232#missing多，业务含义大
    test_woe_住院自付费用[,2],#0.151364058
    test_woe_客户年龄[,2],#0.148165607
    test_woe_住院合计费用[,2],#0.147196675
    test_woe_value门诊费用[,2],#0.141926236
    test_woe_住院统筹支付费用[,2],#0.141689361
    test_woe_业务员所属二级机构ID[,2],#0.126097297
    #  test_woe_保单所属二级机构代码[,2],#0.125553384
    test_woe_value门诊[,2],#0.124547341
    test_woe_客户收入[,2],#0.123574944
    test_woe_西药费.住院[,2],#0.107389178
    #  test_woe_门诊自付费用[,2],#0.106916694,missing过多
    # test_woe_门诊合计费用[,2],#0.104901679,missing过多
    #  test_woe_门诊自费费用[,2],#0.10394099,missing过多
    #  test_woe_西药费.门诊[,2],#0.101836974,missing过多，且无binning,建议删除 
    #  test_woe_检查费.门诊[,2],#0.101836974,missing过多，且无binning,建议删除
    #  test_woe_手术费.门诊[,2],#0.101836974,missing过多，且无binning,建议删除
    #  test_woe_中成药费.住院.1[,2],#0.101836974,missing过多，且无binning,建议删除
    #  test_woe_化验费.住院.1[,2],#0.101836974,missing过多，且无binning,建议删除
    # test_woe_治疗费.门诊[,2],#0.101836974,missing过多，且无binning,建议删除
    # test_woe_中草药费.门诊[,2],#0.101836974,missing过多，且无binning,建议删除
    # test_woe_放射费.门诊[,2],#0.101836974,missing过多，且无binning,建议删除
    # test_woe_诊察费.门诊[,2],#0.101836974,missing过多，且无binning,建议删除
    # test_woe_特殊检查费.门诊[,2],#0.101836974,missing过多，且无binning,建议删除
    # test_woe_材料费.门诊[,2],#0.101836974,missing过多，且无binning,建议删除
    # test_woe_其他.门诊[,2],#0.101836974,missing过多，且无binning,建议删除
    test_woe_此保单年度保费总额[,2],#0.096991722
    test_woe_投保人与主被保人关系[,2],#0.094720919
    test_woe_是否慢性病[,2],#0.087983319
    test_woe_保单人身险保额[,2],#0.083927111
    # test_woe_门诊统筹支付费用[,2],#0.070379323,missing过多，且无binning,建议删除
    test_woe_住院自费费用[,2],#0.069228374
    # test_woe_门诊其他扣除[,2],#0.068280076,missing过多，且无binning,建议删除
    # test_woe_保单重疾险保额[,2],#0.048824591
    test_woe_是否社保[,2],#0.048066129
    test_woe_保单寿险保额[,2]#0.0418334
  )
  
  names(test_woe_all)
  nrow(test_woe_all)
}

################### 5.3 预测数据及模型评估 ####################
# 测试数据预测及模型评估-技术指标
{
  # 预测违约概率
  test_pred <- predict(m1, test_woe_all, type='response')
  summary(test_pred)
  # ks & roc plot
  # type可设定返回的模型评估指标，包括"ks", "lift", "roc", "pr"
  perf_eva(test_20180701_20180715$险种理赔拒赔原因, test_pred, title = "test")
  
  # type可设定返回的模型评估指标，包括"ks", "lift", "roc", "pr"
  confusionMatrix<-as.matrix(table(test_20180716_20180731$险种理赔拒赔原因,predType))
  confusionMatrix
}

# 测试数据预测及模型评估-业务指标
{
  
}

######################################################################
################## 6. 评分卡的制作与评分值得的计算 ###################
######################################################################

# 评分卡与信用评分 
{
  # 默认基础分points0为600,
  # 对应的坏好比odds0为1/19, 
  # 坏好比翻倍的分数pdo为50分
  
  binnings<-c(  
    bins_险种缴费年度,
    bins_险种内部代码,
    bins_案件出险日期减险种生效时间,
    bins_业务年度,
    bins_险种实际缴费期次,
    bins_案件理赔申请日期减险种生效时间,
    bins_责任赔付金额,
    bins_案件出险日期减代理人离职时间,
    bins_案件理赔申请日期减代理人离职时间,
    bins_出单渠道,
    bins_险种保额,
    bins_险种责任,
    bins_标准保障年限或年龄,
    bins_当前案件理赔申请次数,
    bins_保障年期类型,
    bins_历史理赔案件数,
    bins_案件理赔申请日期减案件出险日期,
    bins_历史理赔总额,
    bins_立案时效,
    bins_业务员入司年限,
    bins_业务员直属机构ID,
    bins_险种重疾险保额,
    bins_案件事故性质,
    bins_代理人离职时间减保单生效日期,
    bins_险种主附约别,
    bins_检查费.住院,
    bins_缴费年限或年龄,
    bins_规保客户积分,
    bins_被保人医疗险年度保费总额,
    bins_adj_value101个单投保人变更,
    bins_adj_value102客户基本资料变更,
    bins_adj_value103受益人变更,
    bins_adj_value108个单新增附约,
    bins_adj_value117职业变更,
    bins_adj_value125个单补充告知,
    bins_adj_value161个单生日性别更正,
    bins_adj_value388停效改有效,
    # bins_adj_value401个单普通复效,
    #bins_adj_value415免息复效,
    bins_化验费.住院,
    bins_保单意外险保额,
    bins_住院自付费用,
    bins_客户年龄,
    bins_住院合计费用,
    bins_value门诊费用,
    bins_住院统筹支付费用,
    bins_业务员所属二级机构ID,
    bins_value门诊,
    bins_客户收入,
    bins_西药费.住院,
    bins_此保单年度保费总额,
    bins_投保人与主被保人关系,
    bins_是否慢性病,
    bins_保单人身险保额,
    bins_住院自费费用,
    bins_是否社保,
    bins_保单寿险保额)
  
  
  
  #final cards
  cards<-scorecard(binnings,m1)
  class(cards)
  names(cards)
  head(cards)
  
  
  
  
  ScoreCard<-rbind(
    cards$basepoints[,c(1,2,4)],
    cards$险种缴费年度[,c(1,2,13)],
    cards$险种内部代码[,c(1,2,13)],
    cards$案件出险日期减险种生效时间[,c(1,2,13)],
    cards$业务年度[,c(1,2,13)],
    cards$险种实际缴费期次[,c(1,2,13)],
    cards$案件理赔申请日期减险种生效时间[,c(1,2,13)],
    cards$责任赔付金额[,c(1,2,13)],
    cards$案件出险日期减代理人离职时间[,c(1,2,13)],
    cards$案件理赔申请日期减代理人离职时间[,c(1,2,13)],
    cards$出单渠道[,c(1,2,13)],
    cards$险种保额[,c(1,2,13)],
    cards$险种责任[,c(1,2,13)],
    cards$标准保障年限或年龄[,c(1,2,13)],
    cards$当前案件理赔申请次数[,c(1,2,13)],
    cards$保障年期类型[,c(1,2,13)],
    cards$历史理赔案件数[,c(1,2,13)],
    cards$案件理赔申请日期减案件出险日期[,c(1,2,13)],
    cards$历史理赔总额[,c(1,2,13)],
    cards$立案时效[,c(1,2,13)],
    cards$业务员入司年限[,c(1,2,13)],
    cards$业务员直属机构ID[,c(1,2,13)],
    cards$险种重疾险保额[,c(1,2,13)],
    cards$案件事故性质[,c(1,2,13)],
    cards$代理人离职时间减保单生效日期[,c(1,2,13)],
    cards$险种主附约别[,c(1,2,13)],
    cards$检查费.住院[,c(1,2,13)],
    cards$缴费年限或年龄[,c(1,2,13)],
    cards$规保客户积分[,c(1,2,13)],
    cards$被保人医疗险年度保费总额[,c(1,2,13)],
    cards$value101个单投保人变更[,c(1,2,13)],
    cards$value102客户基本资料变更[,c(1,2,13)],
    cards$value103受益人变更[,c(1,2,13)],
    cards$value108个单新增附约[,c(1,2,13)],
    cards$value117职业变更[,c(1,2,13)],
    cards$value125个单补充告知[,c(1,2,13)],
    cards$value161个单生日性别更正[,c(1,2,13)],
    cards$value388停效改有效[,c(1,2,13)],
    #cards$value401个单普通复效[,c(1,2,13)],
    #cards$value415免息复效[,c(1,2,13)],
    cards$化验费.住院[,c(1,2,13)],
    cards$保单意外险保额[,c(1,2,13)],
    cards$住院自付费用[,c(1,2,13)],
    cards$客户年龄[,c(1,2,13)],
    cards$住院合计费用[,c(1,2,13)],
    cards$value门诊费用[,c(1,2,13)],
    cards$住院统筹支付费用[,c(1,2,13)],
    cards$业务员所属二级机构ID[,c(1,2,13)],
    cards$value门诊[,c(1,2,13)],
    cards$客户收入[,c(1,2,13)],
    cards$西药费.住院[,c(1,2,13)],
    cards$此保单年度保费总额[,c(1,2,13)],
    cards$投保人与主被保人关系[,c(1,2,13)],
    cards$是否慢性病[,c(1,2,13)],
    cards$保单人身险保额[,c(1,2,13)],
    cards$住院自费费用[,c(1,2,13)],
    cards$是否社保[,c(1,2,13)],
    cards$保单寿险保额[,c(1,2,13)]
  )
  
  View(ScoreCard)
  #write.csv(ScoreCard,"scorecard.csv")
}

# 基于评分卡，计算相应的信用评分
{
  # only_total_score 如果为TRUE只返回总评分，FALSE返回各个变量的评分
  train_score <- scorecard_ply(train, cards, only_total_score = TRUE, print_step = 0)
  train_score<- as.data.frame(train_score);nrow(train_score)
  train_pred_prob<-as.data.frame(train_pred);nrow(train_pred_prob)
  train_true_class<-as.data.frame(train$险种理赔拒赔原因);nrow(train_true_class)
  train_compare<-cbind(train_score,train_pred_prob,train_true_class)
  head(train_compare)
  write.csv(train_compare,"train_compare.csv")
  
  
  test_score <- scorecard_ply(test, cards, only_total_score = TRUE, print_step = 0)
  head(train_score)
  write.csv(train_compare)
} 
# 模型稳定性评估
{
  # x_limits, x_tick_break分别指定计算psi时的评分范围与间隔
  perf_psi(
    score = list(train = train_score, test = test_score),
    label = list(train = train$险种理赔拒赔原因, test = test$险种理赔拒赔原因),
    x_limits = c(250, 700), x_tick_break = 50 )
}
#变量稳定度分析
{
  
}


#####################################################################
####################### 7. XGboost模型的建立 ########################
#####################################################################
step2_finalData <- matrix(as.numeric(unlist(step2_finalData02)),nrow=nrow(step2_finalData02))
step2_finalData <- as.data.frame(step2_finalData)
colnames(step2_finalData) <- colnames(step2_finalData02)
y <- as.matrix(step2_finalData[,1])
x <- as.matrix(step2_finalData[,-1])

################### 7.1 XGboost模型的建立 #####################
library(xgboost)
library(readr) #install.packages("readr")
library(stringr)
library(caret)
library(car) #install.packages("car")
# xgboost fitting with arbitrary parameters
xgb_params_1 = list(
  objective = "binary:logistic",                                               # binary classification
  eta = 0.01,                                                                  # learning rate
  max.depth = 3,                                                               # max tree depth
  eval_metric = "auc"                                                          # evaluation/loss metric
)
# fit the model with the arbitrary parameters specified above
xgb_1 = xgboost(data = as.matrix(x),
                label = y,
                params = xgb_params_1,
                nrounds = 100,                                                 # max number of trees to build
                verbose = TRUE,                                         
                print.every.n = 1,
                early.stop.round = 10                                          # stop if no improvement within 10 trees
)
# cross-validate xgboost to get the accurate measure of error
xgb_cv_1 = xgb.cv(params = xgb_params_1,
                  data = as.matrix(x),
                  label = y,
                  nrounds = 100, 
                  nfold = 5,                                                   # number of folds in K-fold
                  prediction = TRUE,                                           # return the prediction using the final model 
                  showsd = TRUE,                                               # standard deviation of loss across folds
                  stratified = TRUE,                                           # sample is unbalanced; use stratified sampling
                  verbose = TRUE,
                  print.every.n = 1, 
                  early.stop.round = 10
)

################### 7.2 XGboost模型的评估 #####################
summary(xgb_1)
xgb.importance(model=xgb_1)
train_pred <- predict(xgb_1, x)
train_pred <- ifelse(train_pred > 0.5,1,0)
summary(train_pred);head(train_pred)
library(scorecard)
yvector <- as.vector(y)
perf_eva(yvector, train_pred, title = "train")

#################### 7.3 测试数据的转换 #######################
testdata <- matrix(as.numeric(unlist(test_finalData)),nrow=nrow(test_finalData))
testdata <- as.data.frame(testdata)
colnames(testdata) <- colnames(test_finalData)
ytest <- as.matrix(testdata[,1])
xtest <- as.matrix(testdata[,-1])

################### 7.4 预测数据及模型评估 ####################
xgb.pred <- predict(xgb_1, xtest)
xgb.pred <- ifelse(xgb.pred > 0.5,1,0)
# Model evaluation based on confusion matrix
trueType <- as.vector(ytest)
predType <- xgb.pred

# ks & roc plot
# type可设定返回的模型评估指标，包括"ks", "lift", "roc", "pr"
perf_eva(trueType, predType, title = "test")

# type可设定返回的模型评估指标，包括"ks", "lift", "roc", "pr"
confusionMatrix<-as.matrix(table(trueType,predType))
confusionMatrix

######################################################################
################# 8. Decision Tree模型的建立 #########################
######################################################################

################### 8.1 Decision Tree模型的建立 #####################
library(lattice)
library(ggplot2)
library(caret)
library(gmodels)
library(rpart)
library(rpart.plot)

DecisionTree <- rpart(step2_finalData02$险种理赔拒赔原因~.,step2_finalData02,method='class')

################### 8.2 Decision Tree模型的评估 #####################
summary(DecisionTree)
#plot the tree
pdf("Decision Tree.pdf", width=10, height=10)
plot(DecisionTree,uniform=TRUE,compress=TRUE,main="Decision Tree")
text(DecisionTree,use.n=T,all=T,cex=0.8)
dev.off()

################### 8.3 预测数据及模型评估 ####################
#prediction
DecisionTree.pr<-predict(DecisionTree,newdata=test_finalData,type="class")

# Model evaluation based on confusion matrix
# Model evaluation based on confusion matrix
trueType <- test_finalData$险种理赔拒赔原因
predType <- DecisionTree.pr

ppe<-function(trueType,predType)
{
  #build confusion matrix
  confusionMatrix<-as.matrix(table(trueType,predType))
  
  #get TP、FN、FP、TN
  TP<-confusionMatrix[2,2] #true positive
  FN<-confusionMatrix[2,1] #false negative
  FP<-confusionMatrix[1,2] #false positive
  TN<-confusionMatrix[1,1] #true negative
  
  #1.Accuracy
  e.A<-TP/(TP+FP)
  
  #2.Negtive Accuracy
  e.NA<-TN/(TN+FN)
  
  #3.Total Accuracy
  e.TA<-(TP+TN)/(TP+FN+FP+TN)
  
  #4.Error Rate
  e.ER<-FP/(TP+FP)
  
  #5.Negtive Error Rate
  e.NER<-FN/(FN+TN)
  
  #6.Total Error Rate
  e.TER<-1-e.TA
  
  #7.Coverage Rate
  e.CR<-TP/(TP+FN)
  
  #8.Negtive Coverage Rate
  e.NCR<-TN/(FP+TN)
  
  #9.FP Rate
  e.FPR<-FP/(FP+TN)
  
  #10.FN Rate
  e.FNR<-FN/(TP+FN)
  
  #11.F value
  e.F<-2*e.A*e.CR/(e.A+e.CR)
  
  #12.Lift Value
  e.LV<-e.A/((TP+FN)/(TP+FN+FP+TN))
  
  #13.correlation coefficient 
  e.phi<-(TP*TN-FP*FN)/sqrt((TP+FN)*(TN+FP)*(TP+FP)*(TN+FN))
  
  #14.Kappa
  pe<-((TP+FN)*(TP+FP)+(FP+TN)*(FN+TN))/(TP+FN+FP+TN)^2
  
  e.Kappa<-(e.TA-pe)/(1-pe)
  
  return(list(e.A=e.A,e.NA=e.NA,e.TA=e.TA,e.ER=e.ER,e.NER=e.NER,e.TER=e.TER,
              e.CR=e.CR,e.NCR=e.NCR,e.FPR=e.FPR,e.FNR=e.FNR,e.F=e.F,e.LV=e.LV,
              e.phi=e.phi,e.Kappa=e.Kappa))
}
ppe(trueType, predType)

#plot roc
library(ROCR)
pred<-prediction(trueType,predType)
perf <- performance(pred,"tpr","fpr")
plot(perf)

#calculate auc
auc.obj <- performance(pred,"auc")
auc <- auc.obj@y.values[[1]]
auc


