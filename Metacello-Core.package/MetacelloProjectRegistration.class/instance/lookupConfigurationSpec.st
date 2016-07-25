lookup
lookupConfigurationSpec
  configurationProjectSpec ifNotNil: [ :spec | ^ spec ].
  ^ baselineProjectSpec