accessing
configurationClasses
    "Return a set of the Metacello configuration classes that have been loaded into the image."

    "self configurationClasses"

    | answer |
    answer := IdentitySet new.
    ConfigurationOf allSubclasses
        do: [ :cl | 
            (cl == BaselineOf or: [ cl inheritsFrom: BaselineOf ])
                ifFalse: [ answer add: cl ] ].
    Object allSubclasses
        do: [ :cl | 
            (answer includes: cl)
                ifFalse: [ 
                    (([ cl isMetacelloConfig ]
                        on: Error
                        do: [ :ex | ex return: false ]) and: [ cl name asString beginsWith: 'ConfigurationOf' ])
                        ifTrue: [ answer add: cl ] ] ].
    ^ answer