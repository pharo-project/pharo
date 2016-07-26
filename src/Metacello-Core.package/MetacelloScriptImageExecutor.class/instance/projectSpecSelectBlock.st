execution callback
projectSpecSelectBlock
    ^ [ :projectSpec | 
    projectSpec
        ifNil: [ false ]
        ifNotNil: [ 
            MetacelloProjectRegistration
                registrationForProjectSpec: projectSpec
                ifAbsent: [ false ]
                ifPresent: [ :existingRegistration :newRegistration | existingRegistration loadedInImage ] ] ]