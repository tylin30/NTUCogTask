cog_datatype <- function(df, Task){
  #which task
  if (Task == "SRTCRT"){
    factorlist <- list("Block", "Trial", "StimulusLocation", "ResponseLocation")
    numericlist <- list("Accuracy", "RT", "SRT", "CRT")
  }
  else if (Task == "MA" || Task =="MAO"){
    factorlist <- list("Block" ,"Trial", "SetSize", "Stimuli", "CorrLocInOrder", "TouchedLoc")
    numericlist <- list("Accuracy", "RT", "yourSPAN")
  }
  else if (Task == "DMS"){
    factorlist <- list("Block", "Trial", "Delay", "StimulusShown", "ProbesShown", "SubjectResponse")
    numericlist <- list("Accuracy", "RT", "delay0ACC", "delay0RT", "delay5ACC", "delay5RT", "delay10ACC", "delay10RT")
  }
  else if (Task == "DR"){
    factorlist <- list("Encode", "Encode1", "Encode2", "Encode3", "Encode4", "Test", "Test1", "Test2", "Test4", "Condition")
    numericlist <- list("ACC", "RT", "e1t1ACC", "e1t1RT", "e1t4ACC", "e1t4RT", "e4t1ACC", "e4t1RT", "e4t4ACC", "e4t4RT", "MeanACC", "MeanRT")
  }
  else if (Task == "SM"){
    factorlist <- list("Block", "Trial", "SpanSize", "StimulusLocationsInOrder", "TouchedLocationsInOrder")
    numericlist <- list("Accuracy", "RT", "yourSPAN")
  }
  else if (Task == "RMS" || Task == "RMO"){
    factorlist <- list("Block", "Trial", "SpanSize", "ListLength", "StimuliShown", "Lure", "CorrRespInOrder", "SubjectResponse")
    numericlist <- list("Accuracy", "RT", "yourSpan")
  }
  else if (Task == "RML"){
    factorlist <- list("Block", "Trial", "SpanSize", "ListLength", "StimulusLocationInOrder", "TouchedLocationsInOrder")
    numericlist <- list("Accuracy", "RT", "yourSpan")
  }
  else if (Task == "SST"){
    factorlist <- list("Block", "Trial", "Condition", "Direction", "Response")
    numericlist <- list("Accuracy", "RT", "pgoACC", "pgoRT", "DelayIntervalStopTrials", "ThresholdStopTrials", "goACC", "goRT", "stopACC")
  }
  #Stroop
  else if (Task == "Sp"){
    factorlist <- list("Block", "Trial", "Condition", "ColorWord", "Color", "Response")
    numericlist <- list("Accuracy", "RT", "ICACC", "ICRT", "NACC", "NRT")
  }
  #Antisaccade
  else if (Task == "As"){
    factorlist <- list("Block", "Trial", "CueLocation", "ArrowDirection", "Response")
    numericlist <- list("Accuracy", "RT", "ACC", "MeanAC", "MeanRT")
  }
  #Color Trail Task
  else if (Task == "CTT"){
    factorlist <- list("Block", "Condition", "responseNumber", "responseColor", "Response")
    numericlist <- list("RT", "nonSwitchACC", "nonSwitchRT", "switchACC", "switchRT", "BlockACC", "BlockRT", "SaAC", "SwitchAC", "SaRT", "SwitchRT")
  }
  #Figure Task
  else if (Task == "Fg"){
    factorlist <- list("Block", "Trial", "Condition", "Task", "Stimulus", "SubjectResponse")
    numericlist <- list("Accuracy", "RT", "BlockRT", "BlockACC", "repACC", "repRT", "swiACC")
  }
  else if (Task == "HF"){
    factorlist <- list("Block", "Trial", "Condition", "StimulusLocation", "SubjectTesponse")
    numericlist <- list("Accuracy", "RT", "BlockACC", "BlockRT", "MixheartACC", "MixheartRT", "MixflowerACC", "MexflowerRT", "GcosthAC", "GcosthRT", "GcostfAC", "GcostfRT")
  }
  else if (Task == "RS"){
    factorlist <- list("Trial", "SpanSize", "Condition", "Digit", "Direction", "DigitResponse", "ArrowShown", "ArrowCollection", "ArrowResponse")
    numericlist <- list("DigitACC", "DigitRT", "AccuracyInOrder", "ArrowRT", "OverallACC", "yourSPAN")
  }



  #common data datatype
  df <- transform(df,
                  Subject = as.factor(Subject),
                  Gender = as.factor(Gender),
                  Age = as.numeric(Age),
                  Education = as.factor(Education),
                  Hand = as.factor(Hand),
                  Seed = as.factor(Seed))

  #input factor list
  for (i in factorlist){
    df[[i]] <- factor(df[[i]])
  }

  #input numeric list
  for (i in numericlist){
    df[[i]] <- as.numeric(df[[i]])
  }

  return(df)
}
