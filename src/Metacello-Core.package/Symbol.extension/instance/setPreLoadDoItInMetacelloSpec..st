*metacello-core
setPreLoadDoItInMetacelloSpec: aMetacelloSpec
    self precedence == 0
        ifTrue: [ self error: 'Invalid selector' ].
    aMetacelloSpec
        setPreLoadDoIt:
            (aMetacelloSpec project valueHolderSpec
                value: self;
                yourself)