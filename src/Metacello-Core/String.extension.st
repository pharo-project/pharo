Extension { #name : 'String' }

{ #category : '*Metacello-Core' }
String >> addToMetacelloRepositories: aMetacelloRepositoriesSpec [

	aMetacelloRepositoriesSpec addMemberForSpec: (MetacelloRepositorySpec description: self)
]

{ #category : '*Metacello-Core' }
String >> setIncludesInMetacelloPackage: aMetacelloPackageSpec [

	aMetacelloPackageSpec setIncludes: { self }
]

{ #category : '*Metacello-Core' }
String >> setLoadsInMetacelloProject: aMetacelloPackageSpec [

	aMetacelloPackageSpec setLoads: { self }.
]

{ #category : '*Metacello-Core' }
String >> setRequiresInMetacelloPackage: aMetacelloPackageSpec [

	aMetacelloPackageSpec setRequires: { self }.
]
