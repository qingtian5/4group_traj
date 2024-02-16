# Load functions

url = "https://raw.githubusercontent.com/bitowaqr/traj/master/Setup_n_functions.R"
source(url)

# prepare data

# Data needs the following format:
# rows = cases
# columns = observations at time points
# no time variables needed

# Enter file path or select manually
setwd("/To/path/") # This is your path
test.data = read.csv("wide_data_norm.csv") # This is your data in your path
# Otherwise select your own data:
# test.data = read.csv(file.choose())

# reducing the size of the demo data set
# test.data = test.data[1:100,] # looking at a subset 

# Missing values need to be coded as negative values for crimCV
test.data[is.na(test.data)] = -0.00001

# data frame to matrix
df = as.matrix(test.data[,-1])
rownames(df) = test.data$ID

# resulting data set
kable(head(df),format="markdown")

# GBTM CLUSTERING: crimCV

## set the grid of models to evaluate
# Set: 
n.cluster = c(1,2,3,4) # 1.which k's to evaluate, 
p.poly = c(1,2) # 2. which p's to evaluate, 
rcv = T # 3. do you want to run cross validation? T/F

## model evaluation

# Run all models
cv.eval.list = list()
index = 0
for(k in n.cluster)
{
  sub.index = 0
  index = index + 1
  cv.eval.list[[index]] = list()
  names(cv.eval.list)[k] = paste("Groups",k,sep="_")
  for(p in p.poly)
  {
    print(cat("\n Running models for", n.cluster[k], "Groups, and",p.poly[p],"polynomials..."))
    cat("\n running k=",k,"poly=",p," \n")
    sub.index = sub.index + 1
    temp = crimCV(df,
                  ng = k,
                  dpolyp = p,
                  rcv = rcv,     
                  model = "ZIP"
    )
    
    cv.eval.list[[index]][[sub.index]] = temp
    
    names(cv.eval.list[[index]])[sub.index] = paste("polynomial",p,sep="_")
  }
}

## retrireve model evaluation results  

# retrieve AIC, BIC and CV error for each of the models
points.for.AIC.plot = points.for.BIC.plot = points.for.cv.plot = data.frame(x=NA,value=NA,cluster=NA)
for(c in 1:length(cv.eval.list)){
  for(p in 1:length(cv.eval.list[[c]])){
    
    
    tryCatch({
      cv = ifelse(!is.null(cv.eval.list[[c]][[p]]$cv),cv.eval.list[[c]][[p]]$cv,NA)
      points.for.cv.plot = rbind(points.for.cv.plot,
                                 data.frame(x=p,value=cv ,cluster=c))
    }, error =function(e){})
    
    
    tryCatch({
      aic = ifelse(!is.null(cv.eval.list[[c]][[p]]$AIC),cv.eval.list[[c]][[p]]$AIC,NA)
      points.for.AIC.plot = rbind(points.for.AIC.plot,
                                  data.frame(x=p,value=aic,cluster=c))
    }, error =function(e){})
    
    tryCatch({
      bic = ifelse(!is.null(cv.eval.list[[c]][[p]]$BIC),cv.eval.list[[c]][[p]]$BIC,NA)
      points.for.BIC.plot = rbind(points.for.BIC.plot,
                                  data.frame(x=p,value=bic,cluster=c))
    }, error =function(e){})
    
  }}

points.for.AIC.plot = points.for.AIC.plot[-1,]  
points.for.BIC.plot = points.for.BIC.plot[-1,] 
points.for.cv.plot = points.for.cv.plot[-1,]

AIC.gbtm.plot = ggplot(points.for.AIC.plot) + 
  geom_line(aes(x=x,y=value,col=as.factor(cluster))) +
  #geom_point(aes(x=x,y=value,col=as.factor(cluster))) +
  geom_text(aes(x=x,y=value,col=as.factor(cluster),label=cluster),size=5) +
  xlab("Polynomial") +
  ylab("AIC")

BIC.gbtm.plot = ggplot(points.for.BIC.plot) + 
  geom_line(aes(x=x,y=value,col=as.factor(cluster))) +
  # geom_point(aes(x=x,y=value,col=as.factor(cluster))) +
  geom_text(aes(x=x,y=value,col=as.factor(cluster),label=cluster),size=5) +
  xlab("Polynomial") +
  ylab("BIC")

cv.error.gbtm.plot = ggplot(points.for.cv.plot) + 
  geom_line(aes(x=x,y=value,col=as.factor(cluster))) +
  geom_text(aes(x=x,y=value,col=as.factor(cluster),label=cluster),size=5) +
  #geom_point(aes(x=x,y=value,col=as.factor(cluster))) +
  xlab("Polynomial")  +
  ylab("LOOCV Absolute error") 

plot.legend = get_legend(AIC.gbtm.plot + theme(legend.position = "bottom"))

model.eval.plot = 
  plot_grid(
    plot_grid(cv.error.gbtm.plot + theme(legend.position = "none"),
              BIC.gbtm.plot + theme(legend.position = "none"),
              AIC.gbtm.plot + theme(legend.position = "none"),
              ncol=3),
    plot.legend,nrow = 2,rel_heights = c(10,1))

# plot model evaluation
model.eval.plot

## Set the parameters for your model of choice

# Select k and p 
k.set = 4
p.set = 1

# plot details
y.axis.label = "Temperature per time"
x.axis.label = "Time"
plot.title = "Temperature per time over the last 3 days"


## Retrive data from your model of choi


# select model
ind.k = which(grepl(k.set,names(cv.eval.list)))
ind.p = which(grepl(p.set,names(cv.eval.list[[ind.k]])))
# retrieve participants membership
gbtm.members = data.frame(ID =  rownames(df),
                          cluster = apply(summary(cv.eval.list[[ind.k]][[ind.p]]),
                                          1,
                                          function(x)which(x ==max(x))))

members.per.cluster = data.frame(table(gbtm.members$cluster))

## Plot estimated trajectories


# estimated trajectories

modelled.list = plot(cv.eval.list[[ind.k]][[ind.p]],size=1,plot=F)
modelled.list$time = modelled.list$time 

# remove normalization (Important)
new_value = (modelled.list$value / 7.8) + 34.2
modelled.list$value = new_value - 1

model.plot.modelled = 
  ggplot(modelled.list) +
  geom_line(aes(x=time,y=value,col=cluster)) +
  scale_y_continuous(name=y.axis.label) +
  scale_x_continuous(name=x.axis.label) +
  ggtitle(plot.title) +
  scale_color_manual(lab=paste("Group ",members.per.cluster$Var1," (n=",members.per.cluster$Freq,")",sep=""),
                     values=c(2,3,4,5),
                     name="Estimated group trajectories") +
  theme_minimal()

model.plot.modelled

## Retrieve group function terms

# retrieve model function terms with intercept and * for p < .05
long.test.dat = melt(df)
names(long.test.dat)  = c("ID","time","value")
long.test.dat$time = as.numeric(gsub("t.","",long.test.dat$time))
long.test.dat = merge(long.test.dat,gbtm.members,"ID")
long.test.dat$cluster = as.factor(long.test.dat$cluster)


# remove normalization (Important)
new_value = (long.test.dat$value / 7.8) + 34.2
long.test.dat$value = new_value - 1


polynomial.model.results = summary(lm(value ~ -1+poly(time,p.set):cluster+cluster, long.test.dat))
model.spec = round(polynomial.model.results$coefficients[,1],2)
sig.model.specification = ifelse(polynomial.model.results$coefficients[,4]<0.05,"*"," ")
model.spec = paste(model.spec,sig.model.specification,sep="")
model.spec = formatC(model.spec)
model.spec = matrix(data=model.spec, ncol=p.set+1)

colnames(model.spec) = c("Intercept",paste("Polynomial",1:p.set))
rownames(model.spec) = c(paste("Group",1:k.set))

kable(model.spec,format="markdown")

## Plot average group trajectories

# Retrieve observed group trajectories
long.test.dat$value[long.test.dat$value<0] = NA
long.test.dat.means = aggregate(value ~ cluster + time, long.test.dat, function(x) mean( x , na.rm = T))


model.plot.from.data = ggplot(long.test.dat.means) +
  geom_line(aes(x=time,y=value,col=cluster)) +
  scale_y_continuous(name=y.axis.label) +
  scale_x_continuous(name=x.axis.label) +
  ggtitle(plot.title) +
  scale_color_manual(lab=paste("Group ",members.per.cluster$Var1," (n=",members.per.cluster$Freq,")",sep=""),
                     values=c(2,3,4,5),
                     name="Observed group trajectories") +
  theme_minimal()

model.plot.from.data 

## Setup for complex plot

# give names to clusters? 
cluster.names = paste(
  c("First cluster", 
    "Second",
    "Third",
    "Fourth"),
  " (n=",format(as.numeric(members.per.cluster$Freq),digits=1),")",sep="")


# set a y limits to have all sub plots on the same scale?
set.y.limit = c(34.5,42)


## Plot group and individual trajectories

# Group average overview and individual trajectories

pop.average.traj = aggregate(value ~time, long.test.dat, function(x) mean(x,na.rm=T))
model.plot.modelled.plus.pop.average = 
  ggplot() +
  geom_line(data=modelled.list,aes(x=time,y=value,col=cluster,linetype="Estimated")) +
  #geom_line(data=pop.average.traj,aes(x=time,y=value,col="Total",linetype="Average")) +  
  scale_y_continuous(name=y.axis.label) +
  scale_x_continuous(name=x.axis.label) +
  ggtitle("Temperature per time") +
  scale_color_manual(lab=c(paste("Group ",members.per.cluster$Var1," (n=",members.per.cluster$Freq,")",sep=""),
                           paste("Total", " (n=",sum(members.per.cluster$Freq),")",sep="")
                           ),
                     values=c(2:(k.set+1),1),
                     name="Estimated group trajectories") +
  #scale_linetype_manual(lab=c("Average","Estimated"),values = c(2,1), name="")+
  guides(color = guide_legend(order = 1),
         linetype = guide_legend(order=0)) +
  theme_minimal()


individual.plot.list = list()
times.ex = unique(long.test.dat$time)

for(i in 1:length(unique(long.test.dat$cluster))){
  individual.plot.list[[i]] = 
    ggplot() +
    theme_minimal() +
    geom_line(data=long.test.dat[long.test.dat$cluster==i,],
              aes(x=time,y=value,group=ID,linetype="Average"),col=i+1,alpha=0.3,size=0.4) +
    geom_line(data=long.test.dat.means[long.test.dat.means$cluster==i,],
              aes(x=time,y=value,linetype="Average"),col=i+1,size=1) +
    geom_line(data=modelled.list[modelled.list$cluster==i,],
              aes(x=time,y=value,col=cluster,linetype="Estimate"),col=i+1,size=1) +
    scale_linetype_manual(lab=c("Average","Estimated"),values = c(2,1), name="") +
    theme(legend.position = "none") +
    ylab("") +
    # ggtitle(plot.titles.for.mega[i]) +
    coord_cartesian(ylim=set.y.limit) 
}
# individual.plot.list[[length(individual.plot.list)+1]] = get.legend

gbtm.mega.plot = 
  plot_grid(model.plot.modelled.plus.pop.average+
              coord_cartesian(ylim=set.y.limit),
            plot_grid(plotlist=individual.plot.list,ncol=round(k.set/2,0)),ncol=1,rel_heights = c(2,3))

gbtm.mega.plot
