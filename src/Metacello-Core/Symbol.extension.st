Extension { #name : #Symbol }

{ #category : #'*metacello-core' }
Symbol >> asMetacelloAttributeList [
    ^ {self}
]

{ #category : #'*metacello-core' }
Symbol >> asMetacelloAttributePath [
    ^ MetacelloMethodSectionPath with: {self}
]

{ #category : #'*metacello-core' }
Symbol >> setForDo: aBlock withInMetacelloConfig: aMetacelloConstructore [
    aMetacelloConstructore setFor: {self} do: aBlock
]

{ #category : #'*metacello-core' }
Symbol >> setForVersion: aString withInMetacelloConfig: aMetacelloConstructore [
    aMetacelloConstructore setFor: {self} version: aString
]

{ #category : #'*metacello-core' }
Symbol >> setPostLoadDoItInMetacelloSpec: aMetacelloSpec [
    self precedence == 0
        ifTrue: [ self error: 'Invalid selector' ].
    aMetacelloSpec
        setPostLoadDoIt:
            (aMetacelloSpec project valueHolderSpec
                value: self;
                yourself)
]

{ #category : #'*metacello-core' }
Symbol >> setPreLoadDoItInMetacelloSpec: aMetacelloSpec [
    self precedence == 0
        ifTrue: [ self error: 'Invalid selector' ].
    aMetacelloSpec
        setPreLoadDoIt:
            (aMetacelloSpec project valueHolderSpec
                value: self;
                yourself)
]
