Extension { #name : #UndefinedObject }

{ #category : #'*metacello-core' }
UndefinedObject >> asMetacelloSemanticVersionNumber [
    ^ MetacelloSemanticVersionNumber fromString: ''
]

{ #category : #'*metacello-core' }
UndefinedObject >> asMetacelloVersionNumber [

	^MetacelloVersionNumber fromString: ''
]

{ #category : #'*metacello-core' }
UndefinedObject >> metacelloRegistrationHash [
    ^ self hash
]

{ #category : #'*metacello-core' }
UndefinedObject >> registrationsCompareEqual: aMetacelloProjectSpec [
    ^ self = aMetacelloProjectSpec
]

{ #category : #'*metacello-core' }
UndefinedObject >> setAuthorInMetacelloVersion: aMetacelloVersionSpec [

	aMetacelloVersionSpec setAuthor: self
]

{ #category : #'*metacello-core' }
UndefinedObject >> setBlessingInMetacelloVersion: aMetacelloVersionSpec [

	aMetacelloVersionSpec setBlessing: self
]

{ #category : #'*metacello-core' }
UndefinedObject >> setDescriptionInMetacelloVersion: aMetacelloVersionSpec [

	aMetacelloVersionSpec setDescription: self
]

{ #category : #'*metacello-core' }
UndefinedObject >> setPostLoadDoItInMetacelloSpec: aMetacelloSpec [

	aMetacelloSpec setPostLoadDoIt: 
		(aMetacelloSpec project valueHolderSpec
			value: self;
			yourself)
]

{ #category : #'*metacello-core' }
UndefinedObject >> setPreLoadDoItInMetacelloSpec: aMetacelloSpec [

	aMetacelloSpec setPreLoadDoIt: 
		(aMetacelloSpec project valueHolderSpec
			value: self;
			yourself)
]

{ #category : #'*metacello-core' }
UndefinedObject >> setProject: aString withInMetacelloConfig: aMetacelloConfig [

	aMetacelloConfig setProject: aString withString: self
]

{ #category : #'*metacello-core' }
UndefinedObject >> setTimestampInMetacelloVersion: aMetacelloVersionSpec [

	aMetacelloVersionSpec setTimestamp: self
]
