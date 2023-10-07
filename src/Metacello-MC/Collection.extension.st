Extension { #name : 'Collection' }

{ #category : '*Metacello-MC' }
Collection >> addToMetacelloRepositories: aMetacelloRepositoriesSpec [

	self do: [:each | each addToMetacelloRepositories: aMetacelloRepositoriesSpec ]
]

{ #category : '*Metacello-MC' }
Collection >> fetchRequiredForMetacelloMCVersion: aMetacelloMCVersion [

	^aMetacelloMCVersion doFetchRequiredFromArray: self.
]

{ #category : '*Metacello-MC' }
Collection >> loadRequiredForMetacelloMCVersion: aMetacelloMCVersion [

	^aMetacelloMCVersion doLoadRequiredFromArray: self.
]

{ #category : '*Metacello-MC' }
Collection >> mergeIntoMetacelloRepositories: aMetacelloRepositoriesSpec [

	self do: [:each | each mergeIntoMetacelloRepositories: aMetacelloRepositoriesSpec ]
]

{ #category : '*Metacello-MC' }
Collection >> recordRequiredForMetacelloMCVersion: aMetacelloMCVersion [

	^aMetacelloMCVersion doRecordRequiredFromArray: self.
]

{ #category : '*Metacello-MC' }
Collection >> removeFromMetacelloRepositories: aMetacelloRepositoriesSpec [

	self do: [:each | each removeFromMetacelloRepositories: aMetacelloRepositoriesSpec ]
]

{ #category : '*Metacello-MC' }
Collection >> resolvePackageSpecsNamedForMetacelloMCVersion: aMetacelloMCVersion visited: visited ifAbsent: aBlock [
    ^ aMetacelloMCVersion
        allPackagesForSpecs: (self collect: [ :ea | aMetacelloMCVersion packageNamed: ea ifAbsent: aBlock ])
        visited: visited
]
