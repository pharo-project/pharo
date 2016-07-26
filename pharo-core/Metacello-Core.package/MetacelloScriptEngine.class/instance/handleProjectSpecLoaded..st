handlers
handleProjectSpecLoaded: exception
    MetacelloProjectRegistration
        registrationForProjectSpec: exception projectSpec
        ifAbsent: [ :new | 
            new
                loadedInImage: true;
                registerProject ]
        ifPresent: [ :existing :new | 
            "unconditionally merge new with existing (updates registration)"
            existing
                copyOnWrite: [ :existingCopy | 
                    existingCopy
                        loadedInImage: true;
                        merge: new ] ].
    exception resume