# R script for a log log plot of data

library(ggplot2)

# Set the working directory

wd = "/Users/tracyt/Desktop/example"
setwd(wd)


# Bring in the data

control <- read.table("Bdf2_annotated_average_counts_control.txt", header=FALSE, sep="\t")
exp <- read.table("Bdf2_annotated_average_counts_experiment.txt", header=FALSE, sep="\t")

# Look at the data

summary(control)
class(control)
str(control)
dim(control)
head(control)

summary(exp)
class(exp)
str(exp)
dim(exp)
head(exp)

# We see that these two data sets each have 83 measures
# We want to compare the control and experimental data for each of these measures
# So we need to make a new data frame that contains both sets of data 
# for each measurement

# We only care about the first and second columns.  Let's take a look at them

control[,1:2]

# We're assuming that each of the 'chr' labels is a measure for that 'chr' and 
# that they are unique.  Let's test.

unique(control[,1])

# It says at the bottom that we have 83 Levels, so 83 unique values. We could
# also count that by seeing the length of the list

length(unique(control[,1]))

# Let's test this for exp too

length(unique(exp[,1]))

# So, we want to merge these dataset
# Again we're assuming that there's one measure for each label in each
# dataset. Let's test that.

# Make a list of the names in the control and experiment
control_name <- control[,1]
exp_name <- exp[,1]

# Let's see how many are in both lists
# 'union' creates a list of all the values that are the same in both lists
inboth <- union(control_name, exp_name)
length(inboth)

# 83 are in both, so we're good!

# So, let's merge the datasets with just the measurement values, and one column
# of the IDs

all <- merge(control[,1:2], exp[,1:3], by="V1")

# Let's take a look at our new data frame
str(all)
head(all)

# We see they have these generic titles V1, V2.x and V2.y
# We can change those to something meaningful so we remember which is which

colnames(all) <- c('chr','control','exp','label')

# Now look at it again
head(all)
summary(all)
# Now we can plot it

plot(all[,2], all[,3])

# or use the names of the columns

plot(all$control, all$exp)

# This doesn't look very good, because it the data is such a range of values
# A log / log plot would be better

plot(log(all$control),log(all$exp))

# Looks better!
# Now let's try it in ggplot because it usually looks nicer

ggplot(all, aes(log(all$control),log(all$exp))) + 
  geom_point()

# Change the axis labels 


ggplot(all, aes(log(control),log(exp))) +
  geom_point() +
  xlab("log(control)") +
  ylab("log(exp)") +
  ggtitle("Control versus Experiment") 

# Add colors

ggplot(all, aes(log(control),log(exp))) +
  geom_point(aes(color=label), size=3) +
  xlab("log(control)") +
  ylab("log(exp)") +
  ggtitle("Control versus Experiment") 

# This looks really nice, but it's hard to tell the difference between the 
# colors. Let's try a fancy colors.

# Now pick a set of colors. 

# See how many categories we have
length(unique(all$label))

colors <- colorRampPalette(c("blue", "red"))(16) 

ggplot(all, aes(log(control),log(exp))) +
  geom_point(aes(color=label), size=3) +
  scale_colour_manual(values=colors) +
  xlab("log(control)") +
  ylab("log(exp)") +
  ggtitle("Control versus Experiment") 


# Actually I should have been doing this differently, and scale things
# rather than stating it explicitly, so this is a good version

ggplot(all, aes(control, exp)) +
  geom_point(aes(color=label), size=3) +
  scale_colour_manual(values=colors) +
  xlab("log(control)") +
  ylab("log(exp)") +
  ggtitle("Control versus Experiment") +
  scale_x_log10() + scale_y_log10()


