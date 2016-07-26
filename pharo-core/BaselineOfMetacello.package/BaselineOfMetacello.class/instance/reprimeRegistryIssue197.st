doits
reprimeRegistryIssue197
  MetacelloScriptEngine defaultRepositoryDescription: nil.
  MetacelloProjectRegistration registry baselineRegistry values
    detect: [ :registration | 
      "inline MetacelloProjectRegistration>>isValid to correct invalid definition ... detecting INVALID registrations"
      registration projectName
        ifNil: [ true ]
        ifNotNil: [ 
          registration configurationProjectSpec
            ifNil: [ registration baselineProjectSpec isNil ]
            ifNotNil: [ registration baselineProjectSpec notNil ] ] ]
    ifNone: [ 
      "no invalid registrations, don't reset and prime"
      ^ self ].
  Transcript
    cr;
    show:
        'priming registry from image. See https://github.com/dalehenrich/metacello-work/issues/197'.
  [ 
  MetacelloProjectRegistration
    resetRegistry;
    primeRegistryFromImage ]
    on: Warning
    do: [ :ex | 
      Transcript
        cr;
        show: 'Warning resumed: ' , ex description.
      ex resume ]