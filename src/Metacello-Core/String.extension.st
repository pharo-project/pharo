Extension { #name : 'String' }

{ #category : '*Metacello-Core' }
String >> addToMetacelloPackages: aMetacelloPackagesSpec [

	| spec |
	spec := aMetacelloPackagesSpec project packageSpec
		        file: self;
		        yourself.
	aMetacelloPackagesSpec addMemberForSpec: spec
]

{ #category : '*Metacello-Core' }
String >> addToMetacelloRepositories: aMetacelloRepositoriesSpec [

	| spec |
	spec := aMetacelloRepositoriesSpec project repositorySpec
		        description: self;
		        yourself.
	aMetacelloRepositoriesSpec addMemberForSpec: spec
]

{ #category : '*Metacello-Core' }
String >> asMetacelloVersionNumber [

	^MetacelloVersionNumber fromString: self
]

{ #category : '*Metacello-Core' }
String >> mergeIntoMetacelloPackages: aMetacelloPackagesSpec [

	| spec |
	spec := aMetacelloPackagesSpec project packageSpec
		        file: self;
		        yourself.
	aMetacelloPackagesSpec mergeMemberForSpec: spec
]

{ #category : '*Metacello-Core' }
String >> mergeIntoMetacelloRepositories: aMetacelloRepositoriesSpec [

	| spec |
	spec := aMetacelloRepositoriesSpec project repositorySpec
		        description: self;
		        yourself.
	aMetacelloRepositoriesSpec mergeMemberForSpec: spec
]

{ #category : '*Metacello-Core-version comparison' }
String >> metacelloIntegerLessThanSelf: anInteger [
	"integer version components are always '>' string component"

	^false
]

{ #category : '*Metacello-Core-version comparison' }
String >> metacelloSemanticIntegerLessThanSelf: anInteger [
    "integer version components are always '>' string component"

    ^ true
]

{ #category : '*Metacello-Core-version comparison' }
String >> metacelloSemanticStringLessThanSelf: aString [
    ^ aString < self
]

{ #category : '*Metacello-Core-version comparison' }
String >> metacelloSemanticVersionComponentLessThan: aMetacelloVersonComponent [
    ^ aMetacelloVersonComponent metacelloSemanticStringLessThanSelf: self
]

{ #category : '*Metacello-Core-version comparison' }
String >> metacelloStringLessThanSelf: aString [

	^aString < self
]

{ #category : '*Metacello-Core-version comparison' }
String >> metacelloVersionComponentLessThan: aMetacelloVersonComponent [

	^aMetacelloVersonComponent metacelloStringLessThanSelf: self
]

{ #category : '*Metacello-Core' }
String >> removeFromMetacelloPackages: aMetacelloPackagesSpec [

	aMetacelloPackagesSpec removeMemberNamed: self
]

{ #category : '*Metacello-Core' }
String >> removeFromMetacelloRepositories: aMetacelloRepositoriesSpec [

	aMetacelloRepositoriesSpec removeMemberNamed: self
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
