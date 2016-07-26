querying
registrationForProjectSpec: aProjectSpec ifAbsent: absentBlock ifPresent: presentBlock
    | newRegistration |
    newRegistration := aProjectSpec asProjectRegistration.
    self registry
        registrationFor: newRegistration
        ifPresent: [ :existing | ^ presentBlock value: existing value: newRegistration ]
        ifAbsent: [ ^ absentBlock value: newRegistration ]