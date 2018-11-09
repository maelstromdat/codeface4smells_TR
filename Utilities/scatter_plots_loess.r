
library(ggplot2)

projects <- list.dirs(path = ".", full.names = FALSE, recursive = FALSE)


devs <- c()
orgsilos <- c()
missinglinks <- c()
radiosilence <- c()
sponsored.devs <- c()
st.congruence <- c()
communicability <- c()
core.global.devs <- c()
core.mail.devs <- c()
core.mail.only.devs <- c()
core.code.devs <- c()
core.code.only.devs <- c()
core.ml.code.devs <- c()
mail.truck  <- c()
code.truck <- c()
core.code.turnover <- c()
mail.mod <- c()
ml.code.devs <- c()


for (prj in projects) {
  repo <- read.csv(paste(prj, "new-report.csv", sep="/"))

  
  orgsilos <- c(orgsilos, repo$norm.org.silo)
  missinglinks <- c(missinglinks, repo$norm.missing.links)
  radiosilence <- c(radiosilence, repo$norm.radio.silence)
  
  devs <- c(devs, repo$devs)
  ml.code.devs <- c(ml.code.devs, repo$ml.code.devs)
  sponsored.devs <- c(sponsored.devs, repo$sponsored.devs)
  core.global.devs <- c(core.global.devs, repo$core.global.devs)
  core.mail.devs <- c(core.mail.devs, repo$core.mail.devs)
  core.code.devs <- c(core.code.devs, repo$core.code.devs)
  core.mail.only.devs <- c(core.mail.only.devs, repo$mail.only.core.devs)
  core.code.only.devs <- c(core.code.only.devs, repo$code.only.core.devs)
  core.ml.code.devs <- c(core.ml.code.devs, repo$ml.code.core.devs)
  
  mail.truck <- c(mail.truck, repo$mail.truck)
  code.truck <- c(code.truck, repo$code.truck)
  
  st.congruence <- c(st.congruence, repo$st.congruence)
  communicability <- c(communicability, repo$communicability)
  
  core.code.turnover <- c(core.code.turnover, repo$core.code.turnover)
  
  mail.mod <- c(mail.mod, repo$mail.mod)
     
}




out_silo <- boxplot.stats(orgsilos)$out
out_missing <- boxplot.stats(missinglinks)$out
out_radio <- boxplot.stats(radiosilence)$out



mean.orgsilos <- mean(orgsilos[!orgsilos %in% out_silo])
mean.missinglinks <- mean(missinglinks[!missinglinks %in% out_missing])
mean.radiosilence <- mean(radiosilence[!radiosilence %in% out_radio])

max.orgsilos <- max(orgsilos[!orgsilos %in% out_silo])
max.missinglinks <- max(missinglinks[!missinglinks %in% out_missing])
max.radiosilence <- max(radiosilence[!radiosilence %in% out_radio])



######################################
## DEVS
######################################

dat <- data.frame(devs = devs,
                  orgsilos = orgsilos)
p <- ggplot(dat[!dat$orgsilos  %in% out_silo, ], aes(x=devs, y=orgsilos)) +   geom_hline(yintercept=mean.orgsilos, color="red") + ylim(0, max.orgsilos) + ylab("Organisational Silo Effect") +
  theme(text = element_text(size=14)) +
  xlab("Global DSN community members") +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth()     # Add a loess smoothed fit curve with confidence region

plot(p)



p <- ggplot(dat[!dat$orgsilos  %in% out_silo, ], aes(x=devs, y=orgsilos)) +   geom_hline(yintercept=mean.orgsilos, color="red") + ylim(0, max.orgsilos) + ylab("Organisational Silo Effect") +
  theme(text = element_text(size=14)) + xlim(0, 200) +
  xlab("Global DSN community members") +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth()     # Add a loess smoothed fit curve with confidence region

plot(p)


dat <- data.frame(devs = devs,
                  missinglinks = missinglinks)
p <- ggplot(dat[!dat$missinglinks  %in% out_missing, ], aes(x=devs, y=missinglinks)) +   geom_hline(yintercept=mean.missinglinks, color="red") + ylim(0, max.missinglinks) + ylab("Missing Links") +
  theme(text = element_text(size=14)) +
  xlab("Global DSN community members") +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth()     # Add a loess smoothed fit curve with confidence region

plot(p)

p <- ggplot(dat[!dat$missinglinks  %in% out_missing, ], aes(x=devs, y=missinglinks)) +   geom_hline(yintercept=mean.missinglinks, color="red") + ylim(0, max.missinglinks) + ylab("Missing Links") +
  theme(text = element_text(size=14)) +
  xlab("Global DSN community members") + xlim(0, 200) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth()     # Add a loess smoothed fit curve with confidence region

plot(p)


dat <- data.frame(devs = devs,
                  radiosilence = radiosilence)
p <- ggplot(dat[!dat$radiosilence  %in% out_radio, ], aes(x=devs, y=radiosilence)) +   geom_hline(yintercept=mean.radiosilence, color="red") + ylim(0, max.radiosilence) + ylab("Radio Silence") +
  theme(text = element_text(size=14)) +
  xlab("Global DSN community members") +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth()     # Add a loess smoothed fit curve with confidence region

plot(p)

p <- ggplot(dat[!dat$radiosilence  %in% out_radio, ], aes(x=devs, y=radiosilence)) +   geom_hline(yintercept=mean.radiosilence, color="red") + ylim(0, max.radiosilence) + ylab("Radio Silence") +
  theme(text = element_text(size=14)) +
  xlab("Global DSN community members") + xlim(0, 200) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth()     # Add a loess smoothed fit curve with confidence region

plot(p)

dat <- data.frame(ml.code.devs = ml.code.devs,
                  radiosilence = radiosilence)
p <- ggplot(dat[!dat$radiosilence  %in% out_radio, ], aes(x=ml.code.devs, y=radiosilence)) +   geom_hline(yintercept=mean.radiosilence, color="red") + ylim(0, max.radiosilence) + ylab("Radio Silence") +
  theme(text = element_text(size=14)) +
  xlab("Collaboration and Communication DSNs community members") +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth()     # Add a loess smoothed fit curve with confidence region

plot(p)

######################################
## Sponsored Devs
######################################

dat <- data.frame(sponsored.devs = sponsored.devs,
                  orgsilos = orgsilos)
p <- ggplot(dat[!dat$orgsilos  %in% out_silo, ], aes(x=sponsored.devs, y=orgsilos)) +   geom_hline(yintercept=mean.orgsilos, color="red") + ylim(0, max.orgsilos) + ylab("Organisational Silo Effect") +
  theme(text = element_text(size=14)) +
  xlab("Sponsored developers") +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth()     # Add a loess smoothed fit curve with confidence region

plot(p)


dat <- data.frame(sponsored.devs = sponsored.devs,
                  missinglinks = missinglinks)
p <- ggplot(dat[!dat$missinglinks  %in% out_missing, ], aes(x=sponsored.devs, y=missinglinks)) +   geom_hline(yintercept=mean.missinglinks, color="red") + ylim(0, max.missinglinks) + ylab("Missing Links") +
  theme(text = element_text(size=14)) +
  xlab("Sponsored developers") +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth()     # Add a loess smoothed fit curve with confidence region

plot(p)



######################################
## CORE DEVS
######################################

dat <- data.frame(core.global.devs = core.global.devs,
                  orgsilos = orgsilos)
p <- ggplot(dat[!dat$orgsilos  %in% out_silo, ], aes(x=core.global.devs, y=orgsilos)) +   geom_hline(yintercept=mean.orgsilos, color="red") + ylim(0, max.orgsilos) + ylab("Organisational Silo Effect") +
  theme(text = element_text(size=14)) +
  xlab("Global DSN core community members") +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth()     # Add a loess smoothed fit curve with confidence region

plot(p)

p <- ggplot(dat[!dat$orgsilos  %in% out_silo, ], aes(x=core.global.devs, y=orgsilos)) +   geom_hline(yintercept=mean.orgsilos, color="red") + ylim(0, max.orgsilos) + ylab("Organisational Silo Effect") +
  theme(text = element_text(size=14)) +
  xlab("Global DSN core community members") + xlim(0, 75) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth()     # Add a loess smoothed fit curve with confidence region

plot(p)


dat <- data.frame(core.global.devs = core.global.devs,
                  missinglinks = missinglinks)
p <- ggplot(dat[!dat$missinglinks  %in% out_missing, ], aes(x=core.global.devs, y=missinglinks)) +   geom_hline(yintercept=mean.missinglinks, color="red") + ylim(0, max.missinglinks) + ylab("Missing Links") +
  theme(text = element_text(size=14)) +
  xlab("Global DSN core community members") +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth()     # Add a loess smoothed fit curve with confidence region

plot(p)


dat <- data.frame(core.global.devs = core.global.devs,
                  radiosilence = radiosilence)
p <- ggplot(dat[!dat$radiosilence  %in% out_radio, ], aes(x=core.global.devs, y=radiosilence)) +   geom_hline(yintercept=mean.radiosilence, color="red") + ylim(0, max.radiosilence) + ylab("Radio Silence") +
  theme(text = element_text(size=14)) +
  xlab("Global DSN core community members") +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth()     # Add a loess smoothed fit curve with confidence region

plot(p)

dat <- data.frame(core.code.devs = core.code.devs,
                  orgsilos = orgsilos)
p <- ggplot(dat[!dat$orgsilos  %in% out_silo, ], aes(x=core.code.devs, y=orgsilos)) +   geom_hline(yintercept=mean.orgsilos, color="red") + ylim(0, max.orgsilos) + ylab("Organisational Silo Effect") +
  theme(text = element_text(size=14)) +
  xlab("Collaboration DSN core community members") +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth()     # Add a loess smoothed fit curve with confidence region

plot(p)


dat <- data.frame(core.code.devs = core.code.devs,
                  missinglinks = missinglinks)
p <- ggplot(dat[!dat$missinglinks  %in% out_missing, ], aes(x=core.code.devs, y=missinglinks)) +   geom_hline(yintercept=mean.missinglinks, color="red") + ylim(0, max.missinglinks) + ylab("Missing Links") +
  theme(text = element_text(size=14)) +
  xlab("Collaboration DSN core community members") +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth()     # Add a loess smoothed fit curve with confidence region

plot(p)


dat <- data.frame(core.mail.devs = core.mail.devs,
                  radiosilence = radiosilence)
p <- ggplot(dat[!dat$radiosilence  %in% out_radio, ], aes(x=core.mail.devs, y=radiosilence)) +   geom_hline(yintercept=mean.radiosilence, color="red") + ylim(0, max.radiosilence) + ylab("Radio Silence") +
  theme(text = element_text(size=14)) +
  xlab("Communication DSN core community members") +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth()     # Add a loess smoothed fit curve with confidence region

plot(p)


dat <- data.frame(code.truck = code.truck,
                  orgsilos = orgsilos)
p <- ggplot(dat[!dat$orgsilos  %in% out_silo, ], aes(x=code.truck, y=orgsilos)) +   geom_hline(yintercept=mean.orgsilos, color="red") + ylim(0, max.orgsilos) + ylab("Organisational Silo Effect") +
  theme(text = element_text(size=14)) +
  xlab("Collaboration DSN truck number") +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth()     # Add a loess smoothed fit curve with confidence region

plot(p)


dat <- data.frame(code.truck = code.truck,
                  missinglinks = missinglinks)
p <- ggplot(dat[!dat$missinglinks  %in% out_missing, ], aes(x=code.truck, y=missinglinks)) +   geom_hline(yintercept=mean.missinglinks, color="red") + ylim(0, max.missinglinks) + ylab("Missing Links") +
  theme(text = element_text(size=14)) +
  xlab("Collaboration DSN truck number") +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth()     # Add a loess smoothed fit curve with confidence region

plot(p)


dat <- data.frame(mail.truck = mail.truck,
                  radiosilence = radiosilence)
p <- ggplot(dat[!dat$radiosilence  %in% out_radio, ], aes(x=mail.truck, y=radiosilence)) +   geom_hline(yintercept=mean.radiosilence, color="red") + ylim(0, max.radiosilence) + ylab("Radio Silence") +
  theme(text = element_text(size=14)) +
  xlab("Communication DSN truck number") +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth()     # Add a loess smoothed fit curve with confidence region

plot(p)


dat <- data.frame(core.code.only.devs = core.code.only.devs,
                  orgsilos = orgsilos)
p <- ggplot(dat[!dat$orgsilos  %in% out_silo, ], aes(x=core.code.only.devs, y=orgsilos)) +   geom_hline(yintercept=mean.orgsilos, color="red") + ylim(0, max.orgsilos) + ylab("Organisational Silo Effect") +
  theme(text = element_text(size=14)) +
  xlab("Collaboration DSN only core community members") +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth()     # Add a loess smoothed fit curve with confidence region

plot(p)


dat <- data.frame(core.code.only.devs = core.code.only.devs,
                  missinglinks = missinglinks)
p <- ggplot(dat[!dat$missinglinks  %in% out_missing, ], aes(x=core.code.only.devs, y=missinglinks)) +   geom_hline(yintercept=mean.missinglinks, color="red") + ylim(0, max.missinglinks) + ylab("Missing Links") +
  theme(text = element_text(size=14)) +
  xlab("Collaboration DSN only core community members") +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth()     # Add a loess smoothed fit curve with confidence region

plot(p)

dat <- data.frame(core.ml.code.devs = core.ml.code.devs,
                  orgsilos = orgsilos)
p <- ggplot(dat[!dat$orgsilos  %in% out_silo, ], aes(x=core.ml.code.devs, y=orgsilos)) +   geom_hline(yintercept=mean.orgsilos, color="red") + ylim(0, max.orgsilos) + ylab("Organisational Silo Effect") +
  theme(text = element_text(size=14)) +
  xlab("Collaboration and Communication DSNs core community members") +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth()     # Add a loess smoothed fit curve with confidence region

plot(p)


dat <- data.frame(core.ml.code.devs = core.ml.code.devs,
                  missinglinks = missinglinks)
p <- ggplot(dat[!dat$missinglinks  %in% out_missing, ], aes(x=core.ml.code.devs, y=missinglinks)) +   geom_hline(yintercept=mean.missinglinks, color="red") + ylim(0, max.missinglinks) + ylab("Missing Links") +
  theme(text = element_text(size=14)) +
  xlab("Collaboration and Communication DSNs core community members") +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth()     # Add a loess smoothed fit curve with confidence region

plot(p)


dat <- data.frame(core.mail.only.devs = core.mail.only.devs,
                  radiosilence = radiosilence)
p <- ggplot(dat[!dat$radiosilence  %in% out_radio, ], aes(x=core.mail.only.devs, y=radiosilence)) +   geom_hline(yintercept=mean.radiosilence, color="red") + ylim(0, max.radiosilence) + ylab("Radio Silence") +
  theme(text = element_text(size=14)) +
  xlab("Communication DSN only core community members") +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth()     # Add a loess smoothed fit curve with confidence region

plot(p)




######################################
## St. congruence
######################################

dat <- data.frame(st.congruence = st.congruence,
                  orgsilos = orgsilos)
p <- ggplot(dat[!dat$orgsilos  %in% out_silo, ], aes(x=st.congruence, y=orgsilos)) +   geom_hline(yintercept=mean.orgsilos, color="red") + ylim(0, max.orgsilos) + ylab("Organisational Silo Effect") +
  theme(text = element_text(size=14)) +
  xlab("Socio-technical congruence") +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth()     # Add a loess smoothed fit curve with confidence region

plot(p)

dat <- data.frame(st.congruence = st.congruence,
                  missinglinks = missinglinks)
p <- ggplot(dat[!dat$missinglinks  %in% out_missing, ], aes(x=st.congruence, y=missinglinks)) +   geom_hline(yintercept=mean.missinglinks, color="red") + ylim(0, max.missinglinks) + ylab("Missing Links") +
  theme(text = element_text(size=14)) +
  xlab("Socio-technical congruence") +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth()     # Add a loess smoothed fit curve with confidence region

plot(p)


######################################
## Communicability
######################################

dat <- data.frame(communicability = communicability,
                  orgsilos = orgsilos)
p <- ggplot(dat[!dat$orgsilos  %in% out_silo, ], aes(x=communicability, y=orgsilos)) +   geom_hline(yintercept=mean.orgsilos, color="red") + ylim(0, max.orgsilos) + ylab("Organisational Silo Effect") +
  theme(text = element_text(size=14)) +
  xlab("Communicability") +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth()     # Add a loess smoothed fit curve with confidence region

plot(p)

dat <- data.frame(communicability = communicability,
                  missinglinks = missinglinks)
p <- ggplot(dat[!dat$missinglinks  %in% out_missing, ], aes(x=communicability, y=missinglinks)) +   geom_hline(yintercept=mean.missinglinks, color="red") + ylim(0, max.missinglinks) + ylab("Missing Links") +
  theme(text = element_text(size=14)) +
  xlab("Communicability") +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth()     # Add a loess smoothed fit curve with confidence region

plot(p)


######################################
## Turnover
######################################

dat <- data.frame(core.code.turnover = core.code.turnover,
                  missinglinks = missinglinks)
p <- ggplot(dat[!dat$missinglinks  %in% out_missing, ], aes(x=core.code.turnover, y=missinglinks)) +   geom_hline(yintercept=mean.missinglinks, color="red") + ylim(0, max.missinglinks) + ylab("Missing Links") +
  theme(text = element_text(size=14)) +
  xlab("Collaboration DSN core developers turnover") +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth()     # Add a loess smoothed fit curve with confidence region

plot(p)


######################################
## Modularity
######################################

dat <- data.frame(mail.mod = mail.mod,
                  radiosilence = radiosilence)
p <- ggplot(dat[!dat$radiosilence  %in% out_radio, ], aes(x=mail.mod, y=radiosilence)) +   geom_hline(yintercept=mean.radiosilence, color="red") + ylim(0, max.radiosilence) + ylab("Radio Silence") +

  theme(text = element_text(size=14)) +
  xlab("Communication DSN modularity") +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth()     # Add a loess smoothed fit curve with confidence region

plot(p)


