api options
onConflictUseIncoming
  self onConflict: [ :ex :loaded :incoming | ex useIncoming ]