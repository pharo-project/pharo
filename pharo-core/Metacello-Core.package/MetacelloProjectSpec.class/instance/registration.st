scripting
registration
    ^ MetacelloProjectRegistration
        registrationForProjectSpec: self
        ifAbsent: [ :ignored |  ]
        ifPresent: [ :existing :new | existing ]