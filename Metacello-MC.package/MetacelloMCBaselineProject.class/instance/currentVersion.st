versions
currentVersion
    ^ self version isSomethingLoaded
        ifTrue: [ self version ]
        ifFalse: [ nil ]