source("../Jiazhou.Startup.R")
jiazhou.startup()

ksocial<-bsrc.checkdatabase2(ptcs$ksocial)
masterdemo<-bsrc.checkdatabase2(ptcs$masterdemo)
idecide_log<-readxl::read_xlsx("~/Box/skinner/data/iDecide subs/Ver2-iDECIDE Participation Flow-COUNT.xlsx")

idmap<-masterdemo$data[c("registration_redcapid","registration_wpicid","registration_initials","registration_soloffid","registration_group","registration_ptcstat___bsocial")]
names(idmap)<-c("masterdemoid","wpicid","initial","soloffid","MD_group","cs_bsocial")
ksub<-ksocial$data[unique(c(2,grep("registration_",names(ksocial$data)),grep("termination_",names(ksocial$data))))]
ksub<-bsrc.findid(ksub,idmap = idmap,id.var = "registration_redcapid")

message("####These following folks are not in masterdemo!! DO SOMETHING!!!!#####")
knotinmaster<-ksub[is.na(ksub$masterdemoid),]
knotinmaster<-knotinmaster[!duplicated(knotinmaster$registration_redcapid),]
knotinmaster
if(nrow(knotinmaster)>0) {stop("ADD THEIR ID TO MASTER DEMO!!!")
  knotinmaster<-knotinmaster[c("registration_redcapid","registration_consentdate","registration_initials")]
  names(knotinmaster)<-c(c("registration_redcapid","reg_condate_ksocial","registration_initials"))
  knotinmaster$registration_ptcstat___ksocial<-1
  redcap_oneshot_upload(knotinmaster,redcap_uri = ptcs$masterdemo$redcap_uri,token = ptcs$masterdemo$token)
  }

message("####THESE PEOPLE DO NOT HAVE SAME K-SOCIAL PROJECT ID AS THEIR MASTERDEMO ID!!!!!##########")
data.frame(KS_ID=ksub$registration_redcapid[which(ksub$registration_redcapid != ksub$masterdemoid)],
           MD_ID=ksub$masterdemoid[which(ksub$registration_redcapid != ksub$masterdemoid)])


ksub<-ksub[!duplicated(ksub$registration_redcapid),]

ksub_att<-ksub[which(ksub$MD_group == "ATT" & ksub$termination_reason___1==1),]
ksub_att[which(ksub_att$redcap_event_name == "consented_arm_2" | ksub_att$cs_bsocial=="1"),]
message("Total number of ATT: ",nrow(ksub_att))
message("Number of K-Social ATT in Bsocial as well: ",length(ksub_att$registration_redcapid[ksub_att$redcap_event_name == "consented_arm_2" | ksub_att$cs_bsocial=="1"]))

MD_kso<-masterdemo$data[which(masterdemo$data$registration_ptcstat___ksocial==1),]
MD_kso<-MD_kso[!MD_kso$registration_redcapid %in% ksub$masterdemoid,]
if(nrow(MD_kso)>0) {
  message("for some reason master demo has unregistred ksocial people....")
  #Not happening now so deal with it later
}
KSocialDF<-data.frame(ID=ksub$masterdemoid,Group=ksub$MD_group,Consent_Date=ksub$registration_consentdate,Type="KSocial",ogID=ksub$registration_redcapid,stringsAsFactors = F)


#iDecide
MD_iDecide<-masterdemo$data[which(masterdemo$data$registration_ptcstat___idecide==1),]
nrow(MD_iDecide)

idecide_logID<-data.frame(ID=na.omit(as.numeric(idecide_log$ID)))
idecide_logID<-bsrc.findid(idecide_logID,idmap = idmap,id.var = "ID")
if(any(is.na(idecide_logID$masterdemoid))) {stop("HEY@!!@@@@@!!!!! ID not in masterdemo@@@!@!!!!!") #do something later
  }

iDecideDF<-data.frame(ID=MD_iDecide$registration_redcapid,Group=MD_iDecide$registration_group,Consent_Date=MD_iDecide$reg_condate_idecide,Type="iDecide",ogID=MD_iDecide$registration_redcapid,stringsAsFactors = F)

JointDF<-rbind(KSocialDF,iDecideDF)
JointDF$ID[is.na(JointDF$ID)]<-JointDF$ogID[is.na(JointDF$ID)]
JointDF$Consent_Date<-as.Date(as.character(JointDF$Consent_Date))
JointDF_wide<-reshape2::dcast(JointDF,ID+Group~Type,value.var = c("Consent_Date"))
JointDF_wide$Consent_Date<-apply(JointDF_wide[c("KSocial","iDecide")],1,function(x){min(na.omit(x))})





