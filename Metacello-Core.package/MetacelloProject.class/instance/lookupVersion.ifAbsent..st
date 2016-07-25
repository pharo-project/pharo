private
lookupVersion: aVersionString ifAbsent: aBlock
    "please use version:...this is a private method"

    ^ self map
        at: aVersionString
        ifAbsent: [ 
            (MetacelloVersionDefinitionError project: self project versionString: aVersionString)
                exception: (self errorMap at: aVersionString ifAbsent: [ ^ aBlock value ]);
                signal ]