api options
onWarningLog
  self
    onWarning: [ :ex | 
      Transcript
        cr;
        show: ex description.
      ex resume ]