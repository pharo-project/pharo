Extension { #name : 'Symbol' }

{ #category : '*Metacello-Core' }
Symbol >> asMetacelloAttributeList [
    ^ {self}
]

{ #category : '*Metacello-Core' }
Symbol >> setForDo: aBlock withInMetacelloConfig: aMetacelloConstructore [
    aMetacelloConstructore setFor: {self} do: aBlock
]

{ #category : '*Metacello-Core' }
Symbol >> setForVersion: aString withInMetacelloConfig: aMetacelloConstructore [
    aMetacelloConstructore setFor: {self} version: aString
]

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
