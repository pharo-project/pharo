lookup
lookupBaselineSpec
  baselineProjectSpec ifNotNil: [ :spec | ^ spec ].
  ^ configurationProjectSpec