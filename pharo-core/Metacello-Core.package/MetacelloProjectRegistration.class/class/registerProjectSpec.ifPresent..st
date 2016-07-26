registration
registerProjectSpec: aProjectSpec ifPresent: presentBlock
    | newRegistration |
    newRegistration := aProjectSpec asProjectRegistration.
    ^ self registry
        registrationFor: newRegistration
        ifPresent: [ :existing | presentBlock value: existing value: newRegistration ]
        ifAbsent: [ newRegistration registerProject ]