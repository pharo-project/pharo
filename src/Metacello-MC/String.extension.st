Extension { #name : #String }

{ #category : #'*metacello-mc' }
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

{ #category : #'*metacello-mc' }
String >> fetchRequiredForMetacelloMCVersion: aMetacelloMCVersion [

	^aMetacelloMCVersion doFetchRequiredFromArray: (Array with: self).
]

{ #category : #'*metacello-mc' }
String >> loadRequiredForMetacelloMCVersion: aMetacelloMCVersion [

	^aMetacelloMCVersion doLoadRequiredFromArray: (Array with: self).
]

{ #category : #'*metacello-mc' }
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

{ #category : #'*metacello-mc' }
String >> packageFileSpecFor: aMetacelloPackagesSpec [

	^(aMetacelloPackagesSpec project packageSpec)
			file: self;
			yourself
]

{ #category : #'*metacello-mc' }
String >> recordRequiredForMetacelloMCVersion: aMetacelloMCVersion [

	^aMetacelloMCVersion doRecordRequiredFromArray: (Array with: self).
]

{ #category : #'*metacello-mc' }
String >> removeFromMetacelloRepositories: aMetacelloRepositoriesSpec [

	aMetacelloRepositoriesSpec addMember: 
		(aMetacelloRepositoriesSpec removeMember 
			name: self;
			yourself)
]

{ #category : #'*metacello-mc' }
String >> resolvePackageSpecsNamedForMetacelloMCVersion: aMetacelloMCVersion visited: visited ifAbsent: aBlock [
    ^ aMetacelloMCVersion
        allPackagesForSpecs: {(aMetacelloMCVersion packageNamed: self ifAbsent: aBlock)}
        visited: visited
]
