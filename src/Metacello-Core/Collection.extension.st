Extension { #name : 'Collection' }

{ #category : '*Metacello-Core' }
Collection >> addToMetacelloRepositories: aMetacelloRepositoriesSpec [

	self do: [:each | each addToMetacelloRepositories: aMetacelloRepositoriesSpec ]
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
