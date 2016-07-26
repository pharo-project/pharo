registration
unregisterProjectRegistration: aMetacelloProjectRegistration
    aMetacelloProjectRegistration configurationProjectSpec
        ifNotNil: [ :spec | 
            self configurationRegistry
                removeKey: spec className
                ifAbsent: [ self error: 'unexpectedly missing project registration' ] ].
    aMetacelloProjectRegistration baselineProjectSpec
        ifNotNil: [ :spec | self baselineRegistry removeKey: spec className ifAbsent: [ self error: 'unexpectedly missing project registration' ] ]