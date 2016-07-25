testing
canDowngradeTo: aProjectRegistration
    "true if there are no load conflicts
        OR
       if the load conflicts involved two cofigurations ONLY and a downgrade is allowed"

    (self hasLoadConflicts: aProjectRegistration)
        ifFalse: [ ^ true ].
    configurationProjectSpec
        ifNotNil: [ 
            aProjectRegistration configurationProjectSpec
                ifNotNil: [ 
                    configurationProjectSpec ensureProjectLoaded.
                    ^ configurationProjectSpec canDowngradeTo: aProjectRegistration configurationProjectSpec ] ].
    ^ false