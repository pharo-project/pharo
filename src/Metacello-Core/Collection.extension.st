Extension { #name : 'Collection' }

{ #category : '*Metacello-Core' }
Collection >> addToMetacelloPackages: aMetacelloPackagesSpec [

	self do: [:each | each addToMetacelloPackages: aMetacelloPackagesSpec ]
]

{ #category : '*Metacello-Core' }
Collection >> addToMetacelloRepositories: aMetacelloRepositoriesSpec [

	self do: [:each | each addToMetacelloRepositories: aMetacelloRepositoriesSpec ]
]

{ #category : '*Metacello-Core-scripting' }
Collection >> execute: projectSpecBlock against: aScriptExecutor [
    aScriptExecutor executeCollection: self do: projectSpecBlock
]

{ #category : '*Metacello-Core' }
Collection >> fetchRequiredForMetacelloMCVersion: aMetacelloMCVersion [

	^aMetacelloMCVersion doFetchRequiredFromArray: self.
]

{ #category : '*Metacello-Core' }
Collection >> loadRequiredForMetacelloMCVersion: aMetacelloMCVersion [

	^aMetacelloMCVersion doLoadRequiredFromArray: self.
]

{ #category : '*Metacello-Core' }
Collection >> mergeIntoMetacelloPackages: aMetacelloPackagesSpec [

	self do: [:each | each mergeIntoMetacelloPackages: aMetacelloPackagesSpec ]
]

{ #category : '*Metacello-Core' }
Collection >> mergeIntoMetacelloRepositories: aMetacelloRepositoriesSpec [

	self do: [:each | each mergeIntoMetacelloRepositories: aMetacelloRepositoriesSpec ]
]

{ #category : '*Metacello-Core' }
Collection >> recordRequiredForMetacelloMCVersion: aMetacelloMCVersion [

	^aMetacelloMCVersion doRecordRequiredFromArray: self.
]

{ #category : '*Metacello-Core' }
Collection >> removeFromMetacelloPackages: aMetacelloPackagesSpec [

	self do: [:each | each removeFromMetacelloPackages: aMetacelloPackagesSpec ]
]

{ #category : '*Metacello-Core' }
Collection >> removeFromMetacelloRepositories: aMetacelloRepositoriesSpec [

	self do: [:each | each removeFromMetacelloRepositories: aMetacelloRepositoriesSpec ]
]

{ #category : '*Metacello-Core' }
Collection >> setIncludesInMetacelloPackage: aMetacelloPackageSpec [

	aMetacelloPackageSpec setIncludes: self asArray.
]

{ #category : '*Metacello-Core' }
Collection >> setLoadsInMetacelloProject: aMetacelloPackageSpec [

	aMetacelloPackageSpec setLoads: self asArray.
]

{ #category : '*Metacello-Core' }
Collection >> setRequiresInMetacelloPackage: aMetacelloPackageSpec [

	aMetacelloPackageSpec setRequires: self asArray.
]
