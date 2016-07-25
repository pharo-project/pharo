registration
registrationFor: aMetacelloProjectRegistration ifPresent: presentBlock ifAbsent: absentBlock
    | baseName |
    baseName := aMetacelloProjectRegistration baseName.
    aMetacelloProjectRegistration configurationProjectSpec
        ifNotNil: [ :spec | self configurationRegistry at: spec className ifPresent: [ :existing | ^ presentBlock value: existing ] ].
    aMetacelloProjectRegistration baselineProjectSpec
        ifNotNil: [ :spec | self baselineRegistry at: spec className ifPresent: [ :existing | ^ presentBlock value: existing ] ].
    self configurationRegistry
        at: 'ConfigurationOf' , baseName
        ifPresent: [ :existing | ^ presentBlock value: existing ].
    self baselineRegistry at: 'BaselineOf' , baseName ifPresent: [ :existing | ^ presentBlock value: existing ].
    ^ absentBlock value