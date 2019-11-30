data.files  <- list.files(path="output",
                         recursive=T,
                         pattern="*.csv"
                         ,full.names=T)
data = lapply(data.files, read.csv)

library(data.table)
data <- rbindlist(data)


library(grid)
library(gridExtra)
library(ggplot2)
library(ggpubr)
library(Rmisc)

# rename mcts to MCTS
levels(data$Algorithm)[levels(data$Algorithm)=="mcts"] <- "MCTS"
# rename minimax to Minimax
levels(data$Algorithm)[levels(data$Algorithm)=="minimax"] <- "Minimax"

#remove NumConditionActionRules == 10 due to incomplete data
#data <- data[NumConditionActionRules!=10]

data$Time = round(data$Time/1000,4)

# The errorbars overlapped, so use position_dodge to move them horizontally
dodge <- position_dodge(0.05) # move them .05 to the left and right
# =============== LanguageSize~Time by Algorithm ===================

# create a group summary
data.language.group <- summarySE(data, measurevar="Time", groupvar=c("Algorithm", "LanguageSize"))
#plot 

figure <- ggplot(data.language.group, aes(x=LanguageSize, y=Time, color=Algorithm)) +   # data
  geom_errorbar(aes(ymin=Time-se,ymax=Time+se), width=.1, position=dodge) + # errorbar
  geom_line(position=dodge) + # line
  #scale_y_continuous(trans="log10") + #log10 scale
  geom_point(position=dodge, size=2, shape=21, fill="white") +
  labs(x="Language Size", y="Time (second)")
show(figure)
ggsave("LanguageSize-Time.png", plot = figure, width=8, height=6, dpi=300)

tbl <- ggtexttable(format(data.language.group,digits=4,scientific = FALSE), rows=NULL, cols=c("Algorithm", "Language\nSize", "N", "Average\nTime (second)", "Standard\nDeviation", "Standard\nError", "Confidence\nInterval (95%)"))
show(tbl)
ggsave("LanguageSize-Time.table.png", plot=tbl)

# =============== NumConditionActionRules~Time by Algorithm ===========
# create a group summary
data.CARules.group <- summarySE(data, measurevar="Time", groupvar=c("Algorithm", "NumConditionActionRules"))

figure <- ggplot(data.CARules.group, aes(x=NumConditionActionRules, y=Time, color=Algorithm)) +   # data
  geom_errorbar(aes(ymin=Time-ci,ymax=Time+ci), width=.1, position=dodge) + # errorbar
  geom_line(position=dodge) + # line
  #scale_y_continuous(trans="log10") + #log10 scale
  geom_point(position=dodge, size=2, shape=21, fill="white") +
  labs(x="Number of Condition Action Rules", y="Time (second)")
show(figure)
ggsave("CARules-Time.png", plot = figure, width=8, height=6, dpi=300)

tbl <- ggtexttable(format(data.CARules.group,digits=4,scientific = FALSE), rows=NULL, cols=c("Algorithm", "Number of Condition-\nAction Rules", "N", "Average\nTime (second)", "Standard\nDeviation", "Standard\nError", "Confidence\nInterval (95%)"))
show(tbl)
ggsave("CARules-Time.table.png", plot=tbl)

# =============== SearchDepth~Time by Algorithm ===========
# create a group summary
data.SearchDepth.group <- summarySE(data, measurevar="Time", groupvar=c("Algorithm", "SearchDepth"))

figure <- ggplot(data.SearchDepth.group, aes(x=SearchDepth, y=Time, color=Algorithm)) +   # data
  geom_errorbar(aes(ymin=Time-ci,ymax=Time+ci), width=.1, position=dodge) + # errorbar
  geom_line(position=dodge) + # line
#  scale_y_continuous(trans="log10") + #log10 scale
  geom_point(position=dodge, size=3, shape=21, fill="white") +
  labs(x="Search Depth", y="Time (second)")
show(figure)
ggsave("SearchDepth-Time.png", plot = figure, width=8, height=6, dpi=300)

tbl <- ggtexttable(format(data.SearchDepth.group,digits=4,scientific = FALSE), rows=NULL, cols=c("Algorithm", "Search Depth", "N", "Average\nTime (second)", "Standard\nDeviation", "Standard\nError", "Confidence\nInterval (95%)"))
show(tbl)
ggsave("SearchDepth-Time.table.png", plot=tbl)


# ================ heatmap LanguageSize - NumConditionActionRules ============

data.LanguageSize.NumCondationActionRules.MCTS.group <- summarySE(data[data$Algorithm=="MCTS"], measurevar = "Time", groupvar=c("LanguageSize", "NumConditionActionRules"))

figure <- ggplot(data.LanguageSize.NumCondationActionRules.MCTS.group, aes(x=LanguageSize, y=NumConditionActionRules, fill=Time)) +
  geom_raster() +
  #scale_fill_gradient(low="#EEEEEE", high="#000000") +
  labs(x="Language Size", y="Number of Condition-Action Rules", fill="Time (second)")

show(figure)

data.LanguageSize.NumCondationActionRules.Minimax.group <- summarySE(data[data$Algorithm=="Minimax"], measurevar = "Time", groupvar=c("LanguageSize", "NumConditionActionRules"))

figure <- ggplot(data.LanguageSize.NumCondationActionRules.Minimax.group, aes(x=LanguageSize, y=NumConditionActionRules, fill=Time)) +
  geom_raster() +
  #scale_fill_gradient(low="#EEEEEE", high="#000000") +
  labs(x="Language Size", y="Number of Condition-Action Rules", fill="Time (second)")

show(figure)

# ================= heatmap LanguageSize - SearchDepth ============

data.LanguageSize.SearchDepth.MCTS.group <- summarySE(data[data$Algorithm=="MCTS"], measurevar = "Time", groupvar=c("LanguageSize", "SearchDepth"))

figure <- ggplot(data.LanguageSize.SearchDepth.MCTS.group, aes(x=LanguageSize, y=SearchDepth, fill=Time)) +
  geom_raster()  +
  #scale_fill_gradient(low="#EEEEEE", high="#000000") +
  labs(x="Language Size", y="Search Depth", fill="Time (second)")

show(figure)

data.LanguageSize.SearchDepth.Minimax.group <- summarySE(data[data$Algorithm=="Minimax"], measurevar = "Time", groupvar=c("LanguageSize", "SearchDepth"))

figure <- ggplot(data.LanguageSize.SearchDepth.Minimax.group, aes(x=LanguageSize, y=SearchDepth, fill=Time)) +
  geom_raster()  +
  #scale_fill_gradient(low="#EEEEEE", high="#000000") +
  labs(x="Language Size", y="Search Depth", fill="Time (second)")

show(figure)

# ================ heatmap SearchDepth - NumConditionActionRules ============
data.SearchDepth.NumCondationActionRules.MCTS.group <- summarySE(data[data$Algorithm=="MCTS"], measurevar = "Time", groupvar=c("SearchDepth", "NumConditionActionRules"))

figure <- ggplot(data.SearchDepth.NumCondationActionRules.MCTS.group, aes(x=SearchDepth, y=NumConditionActionRules, fill=Time)) +
  geom_raster() +
  #scale_fill_gradient(low="#EEEEEE", high="#000000") +
  labs(x="Search Depth", y="Number of Condition-Action Rules", fill="Time (second)")

show(figure)

data.SearchDepth.NumCondationActionRules.Minimax.group <- summarySE(data[data$Algorithm=="Minimax"], measurevar = "Time", groupvar=c("SearchDepth", "NumConditionActionRules"))

figure <- ggplot(data.SearchDepth.NumCondationActionRules.Minimax.group, aes(x=SearchDepth, y=NumConditionActionRules, fill=Time)) +
  geom_raster() +
  #scale_fill_gradient(low="#EEEEEE", high="#000000") +
  labs(x="Search Depth", y="Number of Condition-Action Rules", fill="Time (second)")

show(figure)


# ===========
library(latticeExtra)
mycolors.trans = c("#FF000046", "#00FF0046")
mycolors = c("#FF0000", "#00FF00")


data.LanguageSize.NumCondationActionRules.group <- summarySE(data, measurevar = "Time", groupvar=c("Algorithm", "LanguageSize", "NumConditionActionRules"))
figure <- wireframe(Time~LanguageSize*NumConditionActionRules, data.LanguageSize.NumCondationActionRules.group, 
          groups = Algorithm,
          col.groups=mycolors.trans,
          scales = list(arrows=FALSE, col="black",font=10),
          xlab = list("Language Size",rot=30),
          ylab = list("Number of Condition-Action Rules",rot=-40),
          #auto.key=TRUE,
          key=list(x = .7, y = .85,
                   text=list(c("MCTS","Minimax"),col=mycolors),
                   lines=list(lty=c(1),col=mycolors)),
          par.settings = list(axis.line = list(col = "transparent")))
show(figure)
trellis.device(device="png", filename="LanguageSize-ConditionAction.png",width=1024, 
               height=768)
print(figure)
dev.off()

data.LanguageSize.SearchDepth.group <- summarySE(data, measurevar = "Time", groupvar=c("Algorithm", "LanguageSize", "SearchDepth"))
figure <- wireframe(Time~LanguageSize*SearchDepth, data.LanguageSize.SearchDepth.group,
          groups = Algorithm,
          col.groups=mycolors.trans,
          scales = list(arrows=FALSE, col="black",font=10),
          xlab = list("Language Size",rot=30),
          ylab = list("Search Depth",rot=-40),
          #auto.key=TRUE,
          key=list(x = .7, y = .85,
                   text=list(c("MCTS","Minimax"),col=mycolors),
                   lines=list(lty=c(1),col=mycolors)),
          par.settings = list(axis.line = list(col = "transparent")))
show(figure)
trellis.device(device="png", filename="LanguageSize-SearchDepth.png",width=1024, 
               height=768)
print(figure)
dev.off()

data.SearchDepth.NumCondationActionRules.group <- summarySE(data, measurevar = "Time", groupvar=c("Algorithm", "SearchDepth", "NumConditionActionRules"))
figure <- wireframe(Time~SearchDepth*NumConditionActionRules, data.SearchDepth.NumCondationActionRules.group, 
          groups = Algorithm,
          col.groups=mycolors.trans,
          scales = list(arrows=FALSE, col="black",font=10),
          xlab = list("Search Depth",rot=30),
          ylab = list("Number of Condition-Action Rules",rot=-40),
          #auto.key=TRUE,
          key=list(x = .7, y = .85,
                   text=list(c("MCTS","Minimax"),col=mycolors),
                   lines=list(lty=c(1),col=mycolors)),
          par.settings = list(axis.line = list(col = "transparent")))
show(figure)
trellis.device(device="png", filename="SearchDepth-ConditionAction.png",width=1024, 
               height=768)
print(figure)
dev.off()