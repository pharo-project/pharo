api-configuration
addSymbolicSection: attribute version: versionString
    self methodSpec methodSections
        do: [ :methodSection | 
            methodSection attributes = {attribute}
                ifTrue: [ 
                    methodSection versionString: versionString.
                    ^ self ] ].
    self methodSpec addMethodSection: attribute versionString: versionString