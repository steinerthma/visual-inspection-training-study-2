
library(psych)

###histogram without group differentiation (experimental + control)
##histogram -> effect (Slope / Trend)
#pre_effect (0/0)
hist(dat_subjects$mepre_zz, breaks=10, xlim=c(0,1), ylim=c(0,70), xlab="Bewertungsgenauigkeit", ylab="Personen", main="pre_effect (0 / 0)")

#pre_effect (0/1)
hist(dat_subjects$mepre_zo, breaks=10, xlim=c(0,1), ylim=c(0,70), xlab="Bewertungsgenauigkeit", ylab="Personen", main="pre_effect (0 / 1)")

#pre_effect (1/0)
hist(dat_subjects$mepre_oz, breaks=10, xlim=c(0,1), ylim=c(0,70), xlab="Bewertungsgenauigkeit", ylab="Personen", main="pre_effect (1 / 0)")

#pre_effect (1/1) -> Erweiterung auf 100!
hist(dat_subjects$mepre_oo, breaks=10, xlim=c(0,1), ylim=c(0,100), xlab="Bewertungsgenauigkeit", ylab="Personen", main="pre_effect (1 / 1)")

#post_effect (0/0)
hist(dat_subjects$mepost_zz, breaks=10, xlim=c(0,1), ylim=c(0,70), xlab="Bewertungsgenauigkeit", ylab="Personen", main="post_effect (0 / 0)")

#post_effect (0/1)
hist(dat_subjects$mepost_zo, breaks=10, xlim=c(0,1), ylim=c(0,70), xlab="Bewertungsgenauigkeit", ylab="Personen", main="post_effect (0 / 1)")

#post_effect (1/0)
hist(dat_subjects$mepost_oz, breaks=10, xlim=c(0,1), ylim=c(0,70), xlab="Bewertungsgenauigkeit", ylab="Personen", main="post_effect (1 / 0)")

#post_effect (1/1) -> komische Darstellung (breaks = 8 vs. 10) - warum?
hist(dat_subjects$mepost_oo, breaks=8, xlim=c(0,1), ylim=c(0,70), xlab="Bewertungsgenauigkeit", ylab="Personen", main="post_effect (1 / 1)")

##histogram -> certainty (Slope / Trend)
#pre_certainty (0/0)
hist(dat_subjects$mcpre_zz, breaks=30, xlim=c(0,3), ylim=c(0,20), xlab="Bewertungsgenauigkeit", ylab="Personen", main="pre_certainty (0 / 0)")

#pre_certainty (0/1)
hist(dat_subjects$mcpre_zo, breaks=30, xlim=c(0,3), ylim=c(0,20), xlab="Bewertungsgenauigkeit", ylab="Personen", main="pre_certainty (0 / 1)")

#pre_certainty (1/0)
hist(dat_subjects$mcpre_oz, breaks=30, xlim=c(0,3), ylim=c(0,20), xlab="Bewertungsgenauigkeit", ylab="Personen", main="pre_certainty (1 / 0)")

#pre_certainty (1/1)
hist(dat_subjects$mcpre_oo, breaks=30, xlim=c(0,3), ylim=c(0,20), xlab="Bewertungsgenauigkeit", ylab="Personen", main="pre_certainty (1 / 1)")

#post_certainty (0/0)
hist(dat_subjects$mcpost_zz, breaks=30, xlim=c(0,3), ylim=c(0,20), xlab="Bewertungsgenauigkeit", ylab="Personen", main="post_certainty (0 / 0)")

#post_certainty (0/1)
hist(dat_subjects$mcpost_zo, breaks=30, xlim=c(0,3), ylim=c(0,20), xlab="Bewertungsgenauigkeit", ylab="Personen", main="post_certainty (0 / 1)")

#post_certainty (1/0)
hist(dat_subjects$mcpost_oz, breaks=30, xlim=c(0,3), ylim=c(0,20), xlab="Bewertungsgenauigkeit", ylab="Personen", main="post_certainty(1 / 0)")

#post_certainty (1/1)
hist(dat_subjects$mcpost_oo, breaks=30, xlim=c(0,3), ylim=c(0,30), xlab="Bewertungsgenauigkeit", ylab="Personen", main="post_certainty (1 / 1)")

### continue with histograms with group differenciaten -> especially imp. for post effect