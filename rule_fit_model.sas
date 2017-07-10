/*** Rule Definitions ***/

mod1_rule_001  =  (Sex in ("male")) and (Pclass in ("2","3")) ;
mod1_rule_002  =  (Sex in ("female")) and (Pclass in ("1","2")) ;
mod1_rule_003  =  (Sex in ("female")) and (Pclass in ("1","2")) ;
mod1_rule_004  =  (Sex in ("female")) and (Pclass in ("3")) and (Fare >= 23.7) ;
mod1_rule_005  =  (Sex in ("female")) and (Pclass in ("1","2")) ;
mod1_rule_006  =  (Sex in ("female")) and (Pclass in ("1","2")) ;
mod1_rule_007  =  (Sex in ("male")) and (.z < Fare < 15.1229) ;
mod1_rule_008  =  (Sex in ("female")) and (Pclass in ("1","2")) ;
mod1_rule_009  =  (Sex in ("female")) and (Pclass in ("3")) and (.z < Fare < 7.8875) ;
mod1_rule_010  =  (Sex in ("female")) and (Pclass in ("1","2")) ;
mod1_rule_011  =  (Sex in ("female")) and (.z < SibSp < 2.5) ;
mod1_rule_012  =  (Pclass in ("1","2")) and (Sex in ("female")) ;
mod1_rule_013  =  (Sex in ("male")) and (Age >= 12.5) and (Pclass in ("2","3")) ;
mod1_rule_014  =  (Sex in ("female")) and (Pclass in ("1","2")) ;
mod1_rule_015  =  (Pclass in ("2","3")) and (Sex in ("male")) ;
mod1_rule_016  =  (Sex in ("male")) and (Embarked in ("Q","S")) ;
mod1_rule_017  =  (Sex in ("female")) and (Embarked in ("Q")) ;
mod1_rule_018  =  (Sex in ("male")) and (.z < Age < 6.5) ;
mod1_rule_019  =  (Sex in ("female")) and (Pclass in ("1","2")) ;
mod1_rule_020  =  (Sex in ("male")) and (.z < Fare < 7.9104) ;
mod1_rule_021  =  (Pclass in ("2","3")) and (.z < Age < 9.5) and (.z < SibSp < 1.5) ;
mod1_rule_022  =  (Sex in ("female")) and (.z < Fare < 8.0396) ;
mod1_rule_023  =  (Sex in ("female")) and (Fare >= 8.0396) and (Pclass in ("1","2")) ;
mod1_rule_024  =  (.z < Fare < 52.2771) and (Sex in ("male")) ;
mod1_rule_025  =  (Pclass in ("1")) and (.z < Age < 60.5) ;
mod1_rule_026  =  (.z < Age < 13.5) and (.z < SibSp < 2.5) ;
mod1_rule_027  =  (Embarked in ("S")) and (Pclass in ("3")) ;
mod1_rule_028  =  (.z < Parch < 1.5) and (missing(Age)) and (Sex in ("female")) ;
mod1_rule_029  =  (Fare >= 15.1729) and (.z < SibSp < 1.5) ;
mod1_rule_030  =  (Sex in ("male")) and (.z < Age < 3.5) ;

/*** Model Equation ***/

mod1_rule_fit_model = -0.6114298065 +
  mod1_rule_001 * -0.5182027326 +
  mod1_rule_002 *  1.3407473014 +
  mod1_rule_003 *  0.0260790897 +
  mod1_rule_004 * -0.3355323779 +
  mod1_rule_005 *  0.0008654145 +
  mod1_rule_006 *  0.0000000000 +
  mod1_rule_007 * -0.0634108520 +
  mod1_rule_008 *  0.0001442590 +
  mod1_rule_009 *  0.0139704754 +
  mod1_rule_010 *  0.0015405978 +
  mod1_rule_011 *  0.5118889652 +
  mod1_rule_012 *  0.4263760721 +
  mod1_rule_013 * -0.0161729786 +
  mod1_rule_014 *  0.0000828413 +
  mod1_rule_015 * -0.0098971649 +
  mod1_rule_016 * -0.1197588432 +
  mod1_rule_017 *  0.0190782392 +
  mod1_rule_018 *  0.1501463842 +
  mod1_rule_019 *  0.0012862298 +
  mod1_rule_020 * -0.1042067335 +
  mod1_rule_021 *  0.7960737842 +
  mod1_rule_022 *  0.2002930068 +
  mod1_rule_023 *  0.0001354757 +
  mod1_rule_024 * -0.2590074007 +
  mod1_rule_025 *  0.2044535296 +
  mod1_rule_026 *  0.8395375008 +
  mod1_rule_027 * -0.0828019622 +
  mod1_rule_028 *  0.0996013752 +
  mod1_rule_029 *  0.0455606330 +
  mod1_rule_030 *  0.0650239435
;
