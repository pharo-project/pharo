Extension { #name : 'UndefinedObject' }

{ #category : '*Metacello-Core' }
UndefinedObject >> asMetacelloVersionNumber [

	^MetacelloVersionNumber fromString: ''
]

{ #category : '*Metacello-Core' }
UndefinedObject >> metacelloRegistrationHash [
    ^ self hash
]

{ #category : '*Metacello-Core' }
UndefinedObject >> registrationsCompareEqual: aMetacelloProjectSpec [
    ^ self = aMetacelloProjectSpec
]

{ #category : '*Metacello-Core' }
UndefinedObject >> setPostLoadDoItInMetacelloSpec: aMetacelloSpec [

	aMetacelloSpec setPostLoadDoIt: 
		(aMetacelloSpec project valueHolderSpec
			value: self;
			yourself)
]

{ #category : '*Metacello-Core' }
UndefinedObject >> setPreLoadDoItInMetacelloSpec: aMetacelloSpec [

	aMetacelloSpec setPreLoadDoIt: 
		(aMetacelloSpec project valueHolderSpec
			value: self;
			yourself)
]
