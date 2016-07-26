mutability
copyOnWrite: aBlock
    "assume that only registered projects are immutable ... otherwise you'll get an error"

    | copy |
    self class registry
        registrationFor: self
        ifPresent: [ :existing |  ]
        ifAbsent: [ 
            aBlock value: self.
            ^ self ].
    self unregisterProject.
    copy := self copy.
    aBlock value: copy.
    copy registerProject.
    ^ copy