Extension { #name : 'String' }

{ #category : '*Metacello-Core' }
String >> addToMetacelloPackages: aMetacelloPackagesSpec [

	| spec |
	spec := 
		(aMetacelloPackagesSpec project packageSpec)
			file: self;
			yourself.
	aMetacelloPackagesSpec addMember: 
		(aMetacelloPackagesSpec addMember 
			name: spec name;
			spec: spec;
			yourself)
]

{ #category : '*Metacello-Core' }
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

{ #category : '*Metacello-Core' }
String >> asMetacelloValueHolderFor: aMetacelloVersionSpec [

	^ aMetacelloVersionSpec project valueHolderSpec
		  value: self;
		  yourself
]

{ #category : '*Metacello-Core' }
String >> asMetacelloVersionNumber [

	^MetacelloVersionNumber fromString: self
]

{ #category : '*Metacello-Core' }
String >> execute: projectSpecBlock against: aScriptExecutor [
    aScriptExecutor executeString: self do: projectSpecBlock
]

{ #category : '*Metacello-Core' }
String >> fetchRequiredForMetacelloMCVersion: aMetacelloMCVersion [

	^aMetacelloMCVersion doFetchRequiredFromArray: (Array with: self).
]

{ #category : '*Metacello-Core' }
String >> loadRequiredForMetacelloMCVersion: aMetacelloMCVersion [

	^aMetacelloMCVersion doLoadRequiredFromArray: (Array with: self).
]

{ #category : '*Metacello-Core' }
String >> mergeIntoMetacelloPackages: aMetacelloPackagesSpec [

	| spec |
	spec := 
		(aMetacelloPackagesSpec project packageSpec)
			file: self;
			yourself.
	aMetacelloPackagesSpec addMember: 
		(aMetacelloPackagesSpec mergeMember 
			name: spec name;
			spec: spec;
			yourself)
]

{ #category : '*Metacello-Core' }
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
String >> packageFileSpecFor: aMetacelloPackagesSpec [

	^(aMetacelloPackagesSpec project packageSpec)
			file: self;
			yourself
]

{ #category : '*Metacello-Core' }
String >> recordRequiredForMetacelloMCVersion: aMetacelloMCVersion [

	^aMetacelloMCVersion doRecordRequiredFromArray: (Array with: self).
]

{ #category : '*Metacello-Core' }
String >> removeFromMetacelloPackages: aMetacelloPackagesSpec [

	aMetacelloPackagesSpec addMember: 
		(aMetacelloPackagesSpec removeMember 
			name: self;
			yourself)
]

{ #category : '*Metacello-Core' }
String >> removeFromMetacelloRepositories: aMetacelloRepositoriesSpec [

	aMetacelloRepositoriesSpec addMember: 
		(aMetacelloRepositoriesSpec removeMember 
			name: self;
			yourself)
]

{ #category : '*Metacello-Core' }
String >> setIncludesInMetacelloPackage: aMetacelloPackageSpec [

	aMetacelloPackageSpec setIncludes: { self }.
]

{ #category : '*Metacello-Core' }
String >> setLoadsInMetacelloProject: aMetacelloPackageSpec [

	aMetacelloPackageSpec setLoads: { self }.
]

{ #category : '*Metacello-Core' }
String >> setPostLoadDoItInMetacelloSpec: aMetacelloSpec [
    self asSymbol setPostLoadDoItInMetacelloSpec: aMetacelloSpec
]

{ #category : '*Metacello-Core' }
String >> setPreLoadDoItInMetacelloSpec: aMetacelloSpec [
    self asSymbol setPreLoadDoItInMetacelloSpec: aMetacelloSpec
]

{ #category : '*Metacello-Core' }
String >> setRequiresInMetacelloPackage: aMetacelloPackageSpec [

	aMetacelloPackageSpec setRequires: { self }.
]
