Extension { #name : #Collection }

{ #category : #'*metacello-mc' }
Collection >> addToMetacelloRepositories: aMetacelloRepositoriesSpec [

	self do: [:each | each addToMetacelloRepositories: aMetacelloRepositoriesSpec ]
]

{ #category : #'*metacello-mc' }
Collection >> fetchRequiredForMetacelloMCVersion: aMetacelloMCVersion [

	^aMetacelloMCVersion doFetchRequiredFromArray: self.
]

{ #category : #'*metacello-mc' }
Collection >> loadRequiredForMetacelloMCVersion: aMetacelloMCVersion [

	^aMetacelloMCVersion doLoadRequiredFromArray: self.
]

{ #category : #'*metacello-mc' }
Collection >> mergeIntoMetacelloRepositories: aMetacelloRepositoriesSpec [

	self do: [:each | each mergeIntoMetacelloRepositories: aMetacelloRepositoriesSpec ]
]

{ #category : #'*metacello-mc' }
Collection >> recordRequiredForMetacelloMCVersion: aMetacelloMCVersion [

	^aMetacelloMCVersion doRecordRequiredFromArray: self.
]

{ #category : #'*metacello-mc' }
Collection >> removeFromMetacelloRepositories: aMetacelloRepositoriesSpec [

	self do: [:each | each removeFromMetacelloRepositories: aMetacelloRepositoriesSpec ]
]

{ #category : #'*metacello-mc' }
Collection >> resolvePackageSpecsNamedForMetacelloMCVersion: aMetacelloMCVersion visited: visited ifAbsent: aBlock [
    ^ aMetacelloMCVersion
        allPackagesForSpecs: (self collect: [ :ea | aMetacelloMCVersion packageNamed: ea ifAbsent: aBlock ])
        visited: visited
]
