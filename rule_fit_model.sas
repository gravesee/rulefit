/*** Rule Definitions ***/

mod1_rule_001  =  (Sex in ("female")) and (Pclass in ("1","2")) ;
mod1_rule_002  =  (Sex in ("male")) and (.z < Fare < 26.26875) ;
mod1_rule_003  =  (Sex in ("female")) and (Pclass in ("1","2")) ;
mod1_rule_004  =  (Sex in ("male")) and (.z < Fare < 26.26875) ;
mod1_rule_005  =  (Sex in ("female")) and (.z < SibSp < 2.5) ;
mod1_rule_006  =  (Sex in ("male")) and (Pclass in ("2","3")) ;
mod1_rule_007  =  (Sex in ("female")) and (Pclass in ("1","2")) ;
mod1_rule_008  =  (Sex in ("female")) and (Pclass in ("3")) and (Fare >= 23.35) ;
mod1_rule_009  =  (Sex in ("female")) and (Pclass in ("1","2")) ;
mod1_rule_010  =  (Sex in ("female")) and (Pclass in ("1","2")) ;
mod1_rule_011  =  (Sex in ("female")) and (Embarked in ("C","Q")) ;
mod1_rule_012  =  (Sex in ("male")) and (Pclass in ("2","3")) ;
mod1_rule_013  =  (Sex in ("male")) and (Pclass in ("1")) and (.z < Age < 43) ;
mod1_rule_014  =  (.z < SibSp < 2.5) and (.z < Age < 15.5) and (Sex in ("male")) ;
mod1_rule_015  =  (Age >= 6) and (.z < Fare < 56.1979) and (Age >= 36.25) ;
mod1_rule_016  =  (Fare >= 10.48125) and (.z < SibSp < 2.5) and (.z < Age < 14.25) ;
mod1_rule_017  =  (Fare >= 10.48125) and (.z < SibSp < 2.5) and (missing(Age)) ;
mod1_rule_018  =  (Fare >= 15.1729) and (.z < SibSp < 3.5) ;
mod1_rule_019  =  (Sex in ("female")) and (.z < Fare < 7.8875) ;
mod1_rule_020  =  (Sex in ("female")) and (Fare >= 7.8875) and (Pclass in ("1","2")) ;
mod1_rule_021  =  (missing(Age)) and (Sex in ("female")) and (Embarked in ("C","Q")) ;
mod1_rule_022  =  (Pclass in ("2","3")) and (.z < SibSp < 2.5) and (.z < Age < 8.5) ;
mod1_rule_023  =  (Embarked in ("C")) and (.z < Age < 26.5) ;
mod1_rule_024  =  (Fare >= 7.2396) and (.z < Age < 53) and (Pclass in ("1")) ;

/*** Model Equation ***/

mod1_rule_fit_model = -0.9606228421 +
  mod1_rule_001 *  2.0641075715 +
  mod1_rule_002 * -0.3635631990 +
  mod1_rule_003 *  0.0000101424 +
  mod1_rule_004 * -0.0153457299 +
  mod1_rule_005 *  0.6859869401 +
  mod1_rule_006 * -0.5123737538 +
  mod1_rule_007 *  0.0106528453 +
  mod1_rule_008 * -0.5449249576 +
  mod1_rule_009 *  0.0000418690 +
  mod1_rule_010 *  0.0000000000 +
  mod1_rule_011 *  0.3275325393 +
  mod1_rule_012 * -0.0061665545 +
  mod1_rule_013 *  0.0588462791 +
  mod1_rule_014 *  1.8822087112 +
  mod1_rule_015 * -0.1731705016 +
  mod1_rule_016 *  0.0000603416 +
  mod1_rule_017 *  0.0690471012 +
  mod1_rule_018 *  0.0599756745 +
  mod1_rule_019 *  0.3705372872 +
  mod1_rule_020 *  0.0051213478 +
  mod1_rule_021 *  0.2667739166 +
  mod1_rule_022 *  1.3841167256 +
  mod1_rule_023 *  0.0314834432 +
  mod1_rule_024 *  0.5259447413
;
