Extension { #name : 'UndefinedObject' }

{ #category : '*Metacello-Core' }
UndefinedObject >> asMetacelloSemanticVersionNumber [
    ^ MetacelloSemanticVersionNumber fromString: ''
]

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
UndefinedObject >> setAuthorInMetacelloVersion: aMetacelloVersionSpec [

	aMetacelloVersionSpec setAuthor: self
]

{ #category : '*Metacello-Core' }
UndefinedObject >> setBlessingInMetacelloVersion: aMetacelloVersionSpec [

	aMetacelloVersionSpec setBlessing: self
]

{ #category : '*Metacello-Core' }
UndefinedObject >> setDescriptionInMetacelloVersion: aMetacelloVersionSpec [

	aMetacelloVersionSpec setDescription: self
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

{ #category : '*Metacello-Core' }
UndefinedObject >> setProject: aString withInMetacelloConfig: aMetacelloConfig [

	aMetacelloConfig setProject: aString withString: self
]

{ #category : '*Metacello-Core' }
UndefinedObject >> setTimestampInMetacelloVersion: aMetacelloVersionSpec [

	aMetacelloVersionSpec setTimestamp: self
]
