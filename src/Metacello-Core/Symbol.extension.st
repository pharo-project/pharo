Extension { #name : 'Symbol' }

{ #category : '*Metacello-Core' }
Symbol >> setPostLoadDoItInMetacelloSpec: aMetacelloSpec [
    self precedence == 0
        ifTrue: [ self error: 'Invalid selector' ].
    aMetacelloSpec
        setPostLoadDoIt:
            (aMetacelloSpec project valueHolderSpec
                value: self;
                yourself)
]

{ #category : '*Metacello-Core' }
Symbol >> setPreLoadDoItInMetacelloSpec: aMetacelloSpec [
    self precedence == 0
        ifTrue: [ self error: 'Invalid selector' ].
    aMetacelloSpec
        setPreLoadDoIt:
            (aMetacelloSpec project valueHolderSpec
                value: self;
                yourself)
]
