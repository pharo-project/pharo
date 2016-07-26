registration
registerProjectRegistration: aMetacelloProjectRegistration
  "unconditionally register <newRegistration> ... use with care"

  aMetacelloProjectRegistration configurationProjectSpec copy
    ifNotNil: [ :spec | 
      self configurationRegistry
        at: spec className
        ifPresent: [ :existing | 
          (existing configurationProjectSpec registrationsCompareEqual: spec)
            ifFalse: [ 
              Transcript
                cr;
                show:
                    'REGISTRATION OF INCOMPATABLE PROJECTS: ' , existing printString
                        , ' REPLACED BY '
                        , aMetacelloProjectRegistration printString ] ].
      spec versionOrNil
        ifNotNil: [ :version | 
          "resolve symbolic versions for loaded projects"
          spec versionString: version versionString ].
      aMetacelloProjectRegistration configurationProjectSpec: spec.
      (aMetacelloProjectRegistration respondsTo: #'versionInfo')
        ifTrue: [ aMetacelloProjectRegistration versionInfo setVersionString: spec versionString ].
      spec immutable.
      self configurationRegistry
        at: spec className
        put: aMetacelloProjectRegistration ].
  aMetacelloProjectRegistration baselineProjectSpec
    ifNotNil: [ :spec | 
      self baselineRegistry
        at: spec className
        ifPresent: [ :existing | 
          (existing baselineProjectSpec registrationsCompareEqual: spec)
            ifFalse: [ 
              Transcript
                cr;
                show:
                    'REGISTRATION OF INCOMPATABLE PROJECTS: ' , existing printString
                        , ' REPLACED BY '
                        , aMetacelloProjectRegistration printString ] ].
      (aMetacelloProjectRegistration respondsTo: #'versionInfo')
        ifTrue: [ 
          aMetacelloProjectRegistration versionInfo
            setVersionString: spec repositoryVersionString ].
      spec immutable.
      self baselineRegistry
        at: spec className
        put: aMetacelloProjectRegistration ].
  aMetacelloProjectRegistration immutable