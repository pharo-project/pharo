Extension { #name : 'String' }

{ #category : '*Metacello-MC' }
String >> addToMetacelloRepositories: aMetacelloRepositoriesSpec [

	| spec |
	spec := 
		(aMetacelloRepositoriesSpec project repositorySpec)
			description: self;
			yourself.
	aMetacelloRepositoriesSpec addMember: 
		(aMetacelloRepositoriesSpec addMember 
			name: spec name;
			spec: spec;
			yourself)
]

{ #category : '*Metacello-MC' }
String >> fetchRequiredForMetacelloMCVersion: aMetacelloMCVersion [

	^aMetacelloMCVersion doFetchRequiredFromArray: (Array with: self).
]

{ #category : '*Metacello-MC' }
String >> loadRequiredForMetacelloMCVersion: aMetacelloMCVersion [

	^aMetacelloMCVersion doLoadRequiredFromArray: (Array with: self).
]

{ #category : '*Metacello-MC' }
String >> mergeIntoMetacelloRepositories: aMetacelloRepositoriesSpec [

	| spec |
	spec := 
		(aMetacelloRepositoriesSpec project repositorySpec)
			description: self;
			yourself.
	aMetacelloRepositoriesSpec addMember: 
		(aMetacelloRepositoriesSpec mergeMember 
			name: spec name;
			spec: spec;
			yourself)
]

{ #category : '*Metacello-MC' }
String >> packageFileSpecFor: aMetacelloPackagesSpec [

	^(aMetacelloPackagesSpec project packageSpec)
			file: self;
			yourself
]

{ #category : '*Metacello-MC' }
String >> recordRequiredForMetacelloMCVersion: aMetacelloMCVersion [

	^aMetacelloMCVersion doRecordRequiredFromArray: (Array with: self).
]

{ #category : '*Metacello-MC' }
String >> removeFromMetacelloRepositories: aMetacelloRepositoriesSpec [

	aMetacelloRepositoriesSpec addMember: 
		(aMetacelloRepositoriesSpec removeMember 
			name: self;
			yourself)
]
