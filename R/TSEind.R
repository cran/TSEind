#' @title Mean absolute error (MAE)
#' @description Calculates MAE when Actual# and Survey# have independent samples
#' @param Actual1 data from a "gold standard" survey; data are assumed to be the "actual" response, without survey error
#' @param Survey1 data from another survey, but with survey error; function will calculate MAE for this survey
#' @param ... used for additional surveys with survey error, survey 2 through survey #
#' @return Matrix with MAE values for survey 1 through survey #
#' @examples MAEi(Actual1=TESTIND$A1, Survey1=TESTIND$S1, Actual2=TESTIND$A1, Survey2=TESTIND$S2,
#' Actual3=TESTIND$A2, Survey3=TESTIND$S3)
#' @note Make sure to properly order inputs, per the example: for each survey, inputs
#' must be paired as Actual#, Survey#, and each pair given in sequential order
#' @export MAEi
MAEi=function(Actual1, Survey1, ...){
  #create lists from ...
    other=list(...)
    otherActual=other[grepl("Actual",names(other))]
    otherSurvey=other[grepl("Survey",names(other))]
  #create storage vector
    maevec=vector(length=(length(otherActual)+1))
  #calculate and store Survey1 values
    mae=mean(abs(Actual1-Survey1), na.rm=TRUE)
    maevec[1]=mae
  #calculate and store ... values
    for(i in seq(along=otherActual)){
      maemore=mean(abs(otherActual[[i]]-otherSurvey[[i]]), na.rm=TRUE)
      maevec[i+1]=maemore
    }
  #convert to matrix and label
    maematrix=noquote(cbind(matrix(format(maevec, signif=7))))
    rownames(maematrix)=paste("   survey",
      seq(along=maematrix), " => ")
    colnames(maematrix)=c("MAE")
  #return results
    maematrix
}

#' @title Mean squared error (MSE) with bias-variance decomposition
#' @description Calculates MSE with bias-variance decomposition when Actual# and Survey# have independent samples
#' @param Actual1 data from a "gold standard" survey; data are assumed to be the "actual" response, without survey error
#' @param Survey1 data from another survey, but with survey error; function will calculate MSE with bias-variance decomposition for this survey
#' @param ... used for additional surveys with survey error, survey 2 through survey #
#' @return Matrix with MSE, bias^2, and variance values for survey 1 through survey #
#' @examples MSEi(Actual1=TESTIND$A1, Survey1=TESTIND$S1, Actual2=TESTIND$A1, Survey2=TESTIND$S2,
#' Actual3=TESTIND$A2, Survey3=TESTIND$S3)
#' @note Make sure to properly order inputs, per the example: for each survey, inputs
#' must be paired as Actual#, Survey#, and each pair given in sequential order
#' @export MSEi
MSEi=function(Actual1, Survey1, ...){
  #create lists from ...
    other=list(...)
    otherActual=other[grepl("Actual",names(other))]
    otherSurvey=other[grepl("Survey",names(other))]
  #create storage vector
    msevec=vector(length=(length(otherActual)+1))
    bias2vec=vector(length=(length(otherActual)+1))
    varvec=vector(length=(length(otherActual)+1))
    addvec=vector(length=(length(otherActual)+1))
    addvec[1:(length(otherActual)+1)]=" + "
    eqvec=vector(length=(length(otherActual)+1))
    eqvec[1:(length(otherActual)+1)]=" => "
  #calculate and store Survey1 values
    mse=mean((Actual1-Survey1)^2, na.rm=TRUE)
    msevec[1]=mse
    bias2=(mean(Actual1-Survey1, na.rm=TRUE))^2
    bias2vec[1]=bias2
    var=mean(((Actual1-Survey1)-(mean(Actual1-Survey1, na.rm=TRUE)))^2, na.rm=TRUE)
    varvec[1]=var
  #calculate and store ... values
    for(i in seq(along=otherActual)){
      msemore=mean((otherActual[[i]]-otherSurvey[[i]])^2, na.rm=TRUE)
      msevec[i+1]=msemore
      bias2more=(mean(otherActual[[i]]-otherSurvey[[i]], na.rm=TRUE))^2
      bias2vec[i+1]=bias2more
      varmore=mean(((otherActual[[i]]-otherSurvey[[i]])-
        (mean(otherActual[[i]]-otherSurvey[[i]], na.rm=TRUE)))^2, na.rm=TRUE)
      varvec[i+1]=varmore
    }
  #convert to matrix and label
    msematrix=noquote(matrix(cbind(format(msevec, signif=7), eqvec,
      format(bias2vec, signif=7), addvec, format(varvec, signif=7)), ncol=5))
    rownames(msematrix)=paste("   survey", seq(along=msematrix[, 1]), " => ")
    colnames(msematrix)=c("MSE", " ", "Bias^2", " ", "Var")
  #return results
    msematrix
}

#' @title Root mean squared error (MAE)
#' @description Calculates RMSE when Actual# and Survey# have independent samples
#' @param Actual1 data from a "gold standard" survey; data are assumed to be the "actual" response, without survey error
#' @param Survey1 data from another survey, but with survey error; function will calculate RMSE for this survey
#' @param ... used for additional surveys with survey error, survey 2 through survey #
#' @return Matrix with RMSE values for survey 1 through survey #
#' @examples RMSEi(Actual1=TESTIND$A1, Survey1=TESTIND$S1, Actual2=TESTIND$A1, Survey2=TESTIND$S2,
#' Actual3=TESTIND$A2, Survey3=TESTIND$S3)
#' @note Make sure to properly order inputs, per the example: for each survey, inputs
#' must be paired as Actual#, Survey#, and each pair given in sequential order
#' @export RMSEi
RMSEi=function(Actual1, Survey1, ...){
  #create lists from ...
    other=list(...)
    otherActual=other[grepl("Actual",names(other))]
    otherSurvey=other[grepl("Survey",names(other))]
  #create storage vector
    rmsevec=vector(length=(length(otherActual)+1))
  #calculate and store Survey1 values
    rmse=sqrt(mean((Actual1-Survey1)^2, na.rm=TRUE))
    rmsevec[1]=rmse
  #calculate and store ... values
    for(i in seq(along=otherActual)){
      rmsemore=sqrt(mean((otherActual[[i]]-otherSurvey[[i]])^2, na.rm=TRUE))
      rmsevec[i+1]=rmsemore
    }
  #convert to matrix and label
    rmsematrix=noquote(cbind(matrix(format(rmsevec, signif=7))))
    rownames(rmsematrix)=paste("   survey",
      seq(along=rmsematrix), " => ")
    colnames(rmsematrix)=c("RMSE")
  #return results
    rmsematrix
}

#' @title Mean squared logarithmic error (MSLE)
#' @description Calculates MSLE when Actual# and Survey# have independent samples
#' @param Actual1 data from a "gold standard" survey; data are assumed to be the "actual" response, without survey error
#' @param Survey1 data from another survey, but with survey error; function will calculate MSLE for this survey
#' @param ... used for additional surveys with survey error, survey 2 through survey #
#' @return Matrix with MSLE values for survey 1 through survey #
#' @examples MSLEi(Actual1=TESTIND$A1, Survey1=TESTIND$S1, Actual2=TESTIND$A1, Survey2=TESTIND$S2,
#' Actual3=TESTIND$A2, Survey3=TESTIND$S3)
#' @note Make sure to properly order inputs, per the example: for each survey, inputs
#' must be paired as Actual#, Survey#, and each pair given in sequential order
#' @export MSLEi
MSLEi=function(Actual1, Survey1, ...){
  #create lists from ...
    other=list(...)
    otherActual=other[grepl("Actual",names(other))]
    otherSurvey=other[grepl("Survey",names(other))]
  #create storage vector
    mslevec=vector(length=(length(otherActual)+1))
  #calculate and store Survey1 values
    msle=mean((log(Actual1+1)-log(Survey1+1))^2, na.rm=TRUE)
    mslevec[1]=msle
  #calculate and store ... values
    for(i in seq(along=otherActual)){
      mslemore=mean((log(otherActual[[i]]+1)-
        log(otherSurvey[[i]]+1))^2, na.rm=TRUE)
      mslevec[i+1]=mslemore
    }
  #convert to matrix and label
    mslematrix=noquote(cbind(matrix(format(mslevec, signif=7))))
    rownames(mslematrix)=paste("   survey",
      seq(along=mslematrix), " => ")
    colnames(mslematrix)=c("MSLE")
  #return results
    mslematrix
}

#' @title Root mean squared logarithmic error (RMSLE)
#' @description Calculates RMSLE when Actual# and Survey# have independent samples
#' @param Actual1 data from a "gold standard" survey; data are assumed to be the "actual" response, without survey error
#' @param Survey1 data from another survey, but with survey error; function will calculate RMSLE for this survey
#' @param ... used for additional surveys with survey error, survey 2 through survey #
#' @return Matrix with RMSLE values for survey 1 through survey #
#' @examples RMSLEi(Actual1=TESTIND$A1, Survey1=TESTIND$S1, Actual2=TESTIND$A1, Survey2=TESTIND$S2,
#' Actual3=TESTIND$A2, Survey3=TESTIND$S3)
#' @note Make sure to properly order inputs, per the example: for each survey, inputs
#' must be paired as Actual#, Survey#, and each pair given in sequential order
#' @export RMSLEi
RMSLEi=function(Actual1, Survey1, ...){
  #create lists from ...
    other=list(...)
    otherActual=other[grepl("Actual",names(other))]
    otherSurvey=other[grepl("Survey",names(other))]
  #create storage vector
    rmslevec=vector(length=(length(otherActual)+1))
  #calculate and store Survey1 values
    rmsle=sqrt(mean((log(Actual1+1)-log(Survey1+1))^2, na.rm=TRUE))
    rmslevec[1]=rmsle
  #calculate and store ... values
    for(i in seq(along=otherActual)){
      rmslemore=sqrt(mean((log(otherActual[[i]]+1)-
        log(otherSurvey[[i]]+1))^2, na.rm=TRUE))
      rmslevec[i+1]=rmslemore
    }
  #convert to matrix and label
    rmslematrix=noquote(cbind(matrix(format(rmslevec, signif=7))))
    rownames(rmslematrix)=paste("   survey",
      seq(along=rmslematrix), " => ")
    colnames(rmslematrix)=c("RMSLE")
  #return results
    rmslematrix
}

#' @title Full scale-dependent statistics (MAE, MSE, RMSE, MSLE, and RMSLE)
#' @description Calculates MAE, MSE, RMSE, MSLE, and RMSLE when Actual# and Survey# have independent samples
#' @param Actual1 data from a "gold standard" survey; data are assumed to be the "actual" response, without survey error
#' @param Survey1 data from another survey, but with survey error; function will calculate MAE, MSE, RMSE, MSLE, and RMSLE for this survey
#' @param ... used for additional surveys with survey error, survey 2 through survey #
#' @return Matrix with MAE, MSE, RMSE, MSLE, and RMSLE values for survey 1 through survey #
#' @examples FULLSDi(Actual1=TESTIND$A1, Survey1=TESTIND$S1, Actual2=TESTIND$A1, Survey2=TESTIND$S2,
#' Actual3=TESTIND$A2, Survey3=TESTIND$S3)
#' @note Make sure to properly order inputs, per the example: for each survey, inputs
#' must be paired as Actual#, Survey#, and each pair given in sequential order
#' @export FULLSDi
FULLSDi=function(Actual1, Survey1, ...){
  #create lists from ...
    other=list(...)
    otherActual=other[grepl("Actual",names(other))]
    otherSurvey=other[grepl("Survey",names(other))]
  #create storage vectors
    maevec=vector(length=(length(otherActual)+1))
    msevec=vector(length=(length(otherActual)+1))
    rmsevec=vector(length=(length(otherActual)+1))
    mslevec=vector(length=(length(otherActual)+1))
    rmslevec=vector(length=(length(otherActual)+1))
    spacevec=vector(length=(length(otherActual)+1))
    spacevec[1:(length(otherActual)+1)]=" "
  #calculate and store Survey1 values
    mae=mean(abs(Actual1-Survey1))
    maevec[1]=mae
    mse=mean((Actual1-Survey1)^2)
    msevec[1]=mse
    rmse=sqrt(mean((Actual1-Survey1)^2))
    rmsevec[1]=rmse
    msle=mean((log(Actual1+1)-log(Survey1+1))^2)
    mslevec[1]=msle
    rmsle=sqrt(mean((log(Actual1+1)-log(Survey1+1))^2))
    rmslevec[1]=rmsle
  #calculate and store ... values
    for(i in seq(along=otherActual)){
      maemore=mean(abs(otherActual[[i]]-otherSurvey[[i]]), na.rm=TRUE)
      maevec[i+1]=maemore
      msemore=mean((otherActual[[i]]-otherSurvey[[i]])^2, na.rm=TRUE)
      msevec[i+1]=msemore
      rmsemore=sqrt(mean((otherActual[[i]]-otherSurvey[[i]])^2, na.rm=TRUE))
      rmsevec[i+1]=rmsemore
      mslemore=mean((log(otherActual[[i]]+1)-
        log(otherSurvey[[i]]+1))^2, na.rm=TRUE)
      mslevec[i+1]=mslemore
      rmslemore=sqrt(mean((log(otherActual[[i]]+1)-
        log(otherSurvey[[i]]+1))^2, na.rm=TRUE))
      rmslevec[i+1]=rmslemore
    }
  #convert to matrix and label
    fullsdmatrix=noquote(matrix(cbind(format(maevec, signif=7),
      spacevec, format(msevec, signif=7), spacevec, format(rmsevec,
      signif=7), spacevec, format(mslevec, signif=7), spacevec,
      format(rmslevec, signif=7)), ncol=9))
    rownames(fullsdmatrix)=paste("   survey",
      seq_along(fullsdmatrix[, 1]), " => ")
    colnames(fullsdmatrix)=c("MAE", " ", "MSE", " ", "RMSE",
      " ", "MSLE", " ", "RMSLE")
  #return results
    fullsdmatrix
}

#' @title Mean absolte percentage error (MAPE)
#' @description Calculates MAPE when Actual# and Survey# have independent samples
#' @param Actual1 data from a "gold standard" survey; data are assumed to be the "actual" response, without survey error
#' @param Survey1 data from another survey, but with survey error; function will calculate MAPE for this survey
#' @param ... used for additional surveys with survey error, survey 2 through survey #
#' @return Matrix with MAPE values for survey 1 through survey #
#' @examples MAPEi(Actual1=TESTIND$A1, Survey1=TESTIND$S1, Actual2=TESTIND$A1, Survey2=TESTIND$S2,
#' Actual3=TESTIND$A2, Survey3=TESTIND$S3)
#' @note Make sure to properly order inputs, per the example: for each survey, inputs
#' must be paired as Actual#, Survey#, and each pair given in sequential order
#' @export MAPEi
MAPEi=function(Actual1, Survey1, ...){
  #create lists from ...
    other=list(...)
    otherActual=other[grepl("Actual",names(other))]
    otherSurvey=other[grepl("Survey",names(other))]
  #create storage vector
    mapevec=vector(length=(length(otherActual)+1))
  #calculate and store Survey1 values
    mape=mean(abs((Actual1-Survey1)/Actual1), na.rm=TRUE)
    mapevec[1]=mape
  #calculate and store ... values
    for(i in seq(along=otherActual)){
      mapemore=mean(abs((otherActual[[i]]-
        otherSurvey[[i]])/otherActual[[i]]), na.rm=TRUE)
      mapevec[i+1]=mapemore
    }
  #convert to matrix and label
    mapematrix=noquote(cbind(matrix(format(mapevec, signif=7))))
    rownames(mapematrix)=paste("   survey",
      seq(along=mapematrix), " => ")
    colnames(mapematrix)=c("MAPE")
  #return results
    mapematrix
}

#' @title Symmetric mean absolte percentage error (SMAPE)
#' @description Calculates SMAPE when Actual# and Survey# have independent samples
#' @param Actual1 data from a "gold standard" survey; data are assumed to be the "actual" response, without survey error
#' @param Survey1 data from another survey, but with survey error; function will calculate SMAPE for this survey
#' @param ... used for additional surveys with survey error, survey 2 through survey #
#' @return Matrix with SMAPE values for survey 1 through survey #
#' @examples SMAPEi(Actual1=TESTIND$A1, Survey1=TESTIND$S1, Actual2=TESTIND$A1, Survey2=TESTIND$S2,
#' Actual3=TESTIND$A2, Survey3=TESTIND$S3)
#' @note Make sure to properly order inputs, per the example: for each survey, inputs
#' must be paired as Actual#, Survey#, and each pair given in sequential order
#' @export SMAPEi
SMAPEi=function(Actual1, Survey1, ...){
  #create lists from ...
    other=list(...)
    otherActual=other[grepl("Actual",names(other))]
    otherSurvey=other[grepl("Survey",names(other))]
  #create storage vector
    smapevec=vector(length=(length(otherActual)+1))
  #calculate and store Survey1 values
    smape=mean(abs(Actual1-Survey1)/((abs(Actual1)+abs(Survey1))/2), na.rm=TRUE)
    smapevec[1]=smape
  #calculate and store ... values
    for(i in seq(along=otherActual)){
      smapemore=mean(abs(otherActual[[i]]-otherSurvey[[i]])/
        ((abs(otherActual[[i]])+abs(otherSurvey[[i]]))/2), na.rm=TRUE)
      smapevec[i+1]=smapemore
    }
  #convert to matrix and label
    smapematrix=noquote(cbind(matrix(format(smapevec, signif=7))))
    rownames(smapematrix)=paste("   survey",
      seq(along=smapematrix), " => ")
    colnames(smapematrix)=c("SMAPE")
  #return results
    smapematrix
}

#' @title Relative absolute error (RAE)
#' @description Calculates RAE when Actual# and Survey# have independent samples
#' @param Actual1 data from a "gold standard" survey; data are assumed to be the "actual" response, without survey error
#' @param Survey1 data from another survey, but with survey error; function will calculate RAE for this survey
#' @param ... used for additional surveys with survey error, survey 2 through survey #
#' @return Matrix with RAE values for survey 1 through survey #
#' @examples RAEi(Actual1=TESTIND$A1, Survey1=TESTIND$S1, Actual2=TESTIND$A1, Survey2=TESTIND$S2,
#' Actual3=TESTIND$A2, Survey3=TESTIND$S3)
#' @note Make sure to properly order inputs, per the example: for each survey, inputs
#' must be paired as Actual#, Survey#, and each pair given in sequential order
#' @export RAEi
RAEi=function(Actual1, Survey1, ...){
  #create lists from ...
    other=list(...)
    otherActual=other[grepl("Actual",names(other))]
    otherSurvey=other[grepl("Survey",names(other))]
  #create storage vector
    raevec=vector(length=(length(otherActual)+1))
  #calculate and store Survey1 values
    rae=mean(abs(Actual1-Survey1), na.rm=TRUE)/
      mean(abs(mean(Actual1, na.rm=TRUE)-Actual1), na.rm=TRUE)
    raevec[1]=rae
  #calculate and store ... values
    for(i in seq(along=otherActual)){
      raemore=mean(abs(otherActual[[i]]-otherSurvey[[i]]), na.rm=TRUE)/
        mean(abs(mean(otherActual[[i]], na.rm=TRUE)-otherActual[[i]]), na.rm=TRUE)
      raevec[i+1]=raemore
    }
  #convert to matrix and label
    raematrix=noquote(cbind(matrix(format(raevec, signif=7))))
    rownames(raematrix)=paste("   survey",
      seq(along=raematrix), " => ")
    colnames(raematrix)=c("RAE")
  #return results
    raematrix
}

#' @title Relative squared error (RSE)
#' @description Calculates RSE when Actual# and Survey# have independent samples
#' @param Actual1 data from a "gold standard" survey; data are assumed to be the "actual" response, without survey error
#' @param Survey1 data from another survey, but with survey error; function will calculate RSE for this survey
#' @param ... used for additional surveys with survey error, survey 2 through survey #
#' @return Matrix with RSE values for survey 1 through survey #
#' @examples RSEi(Actual1=TESTIND$A1, Survey1=TESTIND$S1, Actual2=TESTIND$A1, Survey2=TESTIND$S2,
#' Actual3=TESTIND$A2, Survey3=TESTIND$S3)
#' @note Make sure to properly order inputs, per the example: for each survey, inputs
#' must be paired as Actual#, Survey#, and each pair given in sequential order
#' @export RSEi
RSEi=function(Actual1, Survey1, ...){
  #create lists from ...
    other=list(...)
    otherActual=other[grepl("Actual",names(other))]
    otherSurvey=other[grepl("Survey",names(other))]
  #create storage vector
    rsevec=vector(length=(length(otherActual)+1))
  #calculate and store Survey1 values
    rse=mean((Actual1-Survey1)^2, na.rm=TRUE)/
      mean((mean(Actual1, na.rm=TRUE)-Actual1)^2, na.rm=TRUE)
    rsevec[1]=rse
  #calculate and store ... values
    for(i in seq(along=otherActual)){
      rsemore=mean((otherActual[[i]]-otherSurvey[[i]])^2, na.rm=TRUE)/
        mean((mean(otherActual[[i]], na.rm=TRUE)-otherActual[[i]])^2, na.rm=TRUE)
      rsevec[i+1]=rsemore
    }
  #convert to matrix and label
    rsematrix=noquote(cbind(matrix(format(rsevec, signif=7))))
    rownames(rsematrix)=paste("   survey",
      seq(along=rsematrix), " => ")
    colnames(rsematrix)=c("RSE")
  #return results
    rsematrix
}

#' @title Root relative squared error (RRSE)
#' @description Calculates RRSE when Actual# and Survey# have independent samples
#' @param Actual1 data from a "gold standard" survey; data are assumed to be the "actual" response, without survey error
#' @param Survey1 data from another survey, but with survey error; function will calculate RRSE for this survey
#' @param ... used for additional surveys with survey error, survey 2 through survey #
#' @return Matrix with RRSE values for survey 1 through survey #
#' @examples RRSEi(Actual1=TESTIND$A1, Survey1=TESTIND$S1, Actual2=TESTIND$A1, Survey2=TESTIND$S2,
#' Actual3=TESTIND$A2, Survey3=TESTIND$S3)
#' @note Make sure to properly order inputs, per the example: for each survey, inputs
#' must be paired as Actual#, Survey#, and each pair given in sequential order
#' @export RRSEi
RRSEi=function(Actual1, Survey1, ...){
  #create lists from ...
    other=list(...)
    otherActual=other[grepl("Actual",names(other))]
    otherSurvey=other[grepl("Survey",names(other))]
  #create storage vector
    rrsevec=vector(length=(length(otherActual)+1))
  #calculate and store Survey1 values
    rrse=sqrt(mean((Actual1-Survey1)^2, na.rm=TRUE)/
      mean((mean(Actual1, na.rm=TRUE)-Actual1)^2, na.rm=TRUE))
    rrsevec[1]=rrse
  #calculate and store ... values
    for(i in seq(along=otherActual)){
      rrsemore=sqrt(mean((otherActual[[i]]-otherSurvey[[i]])^2, na.rm=TRUE)/
        mean((mean(otherActual[[i]], na.rm=TRUE)-otherActual[[i]])^2, na.rm=TRUE))
      rrsevec[i+1]=rrsemore
    }
  #convert to matrix and label
    rrsematrix=noquote(cbind(matrix(format(rrsevec, signif=7))))
    rownames(rrsematrix)=paste("   survey",
      seq(along=rrsematrix), " => ")
    colnames(rrsematrix)=c("RRSE")
  #return results
    rrsematrix
}

#' @title Full scale-independent statistics (MAPE, SMAPE, RAE, RSE, and RRSE)
#' @description Calculates MAPE, SMAPE, RAE, RSE, and RRSE when Actual# and Survey# have independent samples
#' @param Actual1 data from a "gold standard" survey; data are assumed to be the "actual" response, without survey error
#' @param Survey1 data from another survey, but with survey error; function will calculate MAPE, SMAPE, RAE, RSE, and RRSE for this survey
#' @param ... used for additional surveys with survey error, survey 2 through survey #
#' @return Matrix with MAPE, SMAPE, RAE, RSE, and RRSE values for survey 1 through survey #
#' @examples FULLSIi(Actual1=TESTIND$A1, Survey1=TESTIND$S1, Actual2=TESTIND$A1, Survey2=TESTIND$S2,
#' Actual3=TESTIND$A2, Survey3=TESTIND$S3)
#' @note Make sure to properly order inputs, per the example: for each survey, inputs
#' must be paired as Actual#, Survey#, and each pair given in sequential order
#' @export FULLSIi
FULLSIi=function(Actual1, Survey1, ...){
  #create lists from ...
    other=list(...)
    otherActual=other[grepl("Actual",names(other))]
    otherSurvey=other[grepl("Survey",names(other))]
  #create storage vectors
    mapevec=vector(length=(length(otherActual)+1))
    smapevec=vector(length=(length(otherActual)+1))
    raevec=vector(length=(length(otherActual)+1))
    rsevec=vector(length=(length(otherActual)+1))
    rrsevec=vector(length=(length(otherActual)+1))
    spacevec=vector(length=(length(otherActual)+1))
    spacevec[1:(length(otherActual)+1)]=" "
  #calculate and store Survey1 values
    mape=mean(abs((Actual1-Survey1)/Actual1), na.rm=TRUE)
    mapevec[1]=mape
    smape=mean(abs(Actual1-Survey1)/((abs(Actual1)+abs(Survey1))/2), na.rm=TRUE)
    smapevec[1]=smape
    rae=mean(abs(Actual1-Survey1), na.rm=TRUE)/
      mean(abs(mean(Actual1, na.rm=TRUE)-Actual1), na.rm=TRUE)
    raevec[1]=rae
    rse=mean((Actual1-Survey1)^2, na.rm=TRUE)/
      mean((mean(Actual1, na.rm=TRUE)-Actual1)^2, na.rm=TRUE)
    rsevec[1]=rse
    rrse=sqrt(mean((Actual1-Survey1)^2, na.rm=TRUE)/
      mean((mean(Actual1, na.rm=TRUE)-Actual1)^2, na.rm=TRUE))
    rrsevec[1]=rrse
  #calculate and store ... values
    for(i in seq(along=otherActual)){
      mapemore=mean(abs((otherActual[[i]]-
        otherSurvey[[i]])/otherActual[[i]]), na.rm=TRUE)
      mapevec[i+1]=mapemore
      smapemore=mean(abs(otherActual[[i]]-otherSurvey[[i]])/
        ((abs(otherActual[[i]])+abs(otherSurvey[[i]]))/2), na.rm=TRUE)
      smapevec[i+1]=smapemore
      raemore=mean(abs(otherActual[[i]]-otherSurvey[[i]]), na.rm=TRUE)/
        mean(abs(mean(otherActual[[i]], na.rm=TRUE)-otherActual[[i]]), na.rm=TRUE)
      raevec[i+1]=raemore
      rsemore=mean((otherActual[[i]]-otherSurvey[[i]])^2, na.rm=TRUE)/
        mean((mean(otherActual[[i]], na.rm=TRUE)-otherActual[[i]])^2, na.rm=TRUE)
      rsevec[i+1]=rsemore
      rrsemore=sqrt(mean((otherActual[[i]]-otherSurvey[[i]])^2, na.rm=TRUE)/
        mean((mean(otherActual[[i]], na.rm=TRUE)-otherActual[[i]])^2, na.rm=TRUE))
      rrsevec[i+1]=rrsemore
    }
  #convert to matrix and label
    fullsimatrix=noquote(matrix(cbind(format(mapevec, signif=7),
      spacevec, format(smapevec, signif=7), spacevec, format(raevec,
      signif=7), spacevec, format(rsevec, signif=7), spacevec,
      format(rrsevec, signif=7)), ncol=9))
    rownames(fullsimatrix)=paste("   survey",
      seq_along(fullsimatrix[, 1]), " => ")
    colnames(fullsimatrix)=c("MAPE", " ", "SMAPE", " ", "RAE",
      " ", "RSE", " ", "RRSE")
  #return results
    fullsimatrix
}
