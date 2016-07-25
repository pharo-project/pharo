scripting
unregisterProject
    ^ MetacelloProjectRegistration
        registrationForProjectSpec: self
        ifAbsent: [  ]
        ifPresent: [ :existing :new | existing unregisterProject ]