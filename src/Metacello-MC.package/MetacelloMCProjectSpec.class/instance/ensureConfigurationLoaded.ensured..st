loading
ensureConfigurationLoaded: vrsn ensured: ensured
  "answer true if the configuration should be reloaded:
    blessing is #development
    symbolic version (https://github.com/dalehenrich/metacello-work/issues/283)"

  (vrsn blessing == #'development' or: [ self versionString isSymbol ])
    ifTrue: [ 
      ensured ~~ #'latest'
        ifTrue: [ ^ MetacelloScriptEnsureProjectLoadedForDevelopment signal ] ].
  ^ false