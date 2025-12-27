Extension { #name : 'UndefinedObject' }

{ #category : '*Metacello-Core' }
UndefinedObject >> asMetacelloVersionNumber [

	^MetacelloVersionNumber fromString: ''
]

{ #category : '*Metacello-Core' }
UndefinedObject >> metacelloRegistrationHash [
    ^ self hash
]
