Extension { #name : #String }

{ #category : #'*metacello-core' }
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

{ #category : #'*metacello-core' }
String >> asMetacelloSemanticVersionNumber [
    ^ MetacelloSemanticVersionNumber fromString: self
]

{ #category : #'*metacello-core' }
String >> asMetacelloVersionNumber [

	^MetacelloVersionNumber fromString: self
]

{ #category : #'*metacello-core-scripting' }
String >> execute: projectSpecBlock against: aScriptExecutor [
    aScriptExecutor executeString: self do: projectSpecBlock
]

{ #category : #'*metacello-core' }
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

{ #category : #'*metacello-core-version comparison' }
String >> metacelloIntegerLessThanSelf: anInteger [
	"integer version components are always '>' string component"

	^false
]

{ #category : #'*metacello-core-version comparison' }
String >> metacelloSemanticIntegerLessThanSelf: anInteger [
    "integer version components are always '>' string component"

    ^ true
]

{ #category : #'*metacello-core-version comparison' }
String >> metacelloSemanticStringLessThanSelf: aString [
    ^ aString < self
]

{ #category : #'*metacello-core-version comparison' }
String >> metacelloSemanticVersionComponentLessThan: aMetacelloVersonComponent [
    ^ aMetacelloVersonComponent metacelloSemanticStringLessThanSelf: self
]

{ #category : #'*metacello-core-version comparison' }
String >> metacelloStringLessThanSelf: aString [

	^aString < self
]

{ #category : #'*metacello-core-version comparison' }
String >> metacelloVersionComponentLessThan: aMetacelloVersonComponent [

	^aMetacelloVersonComponent metacelloStringLessThanSelf: self
]

{ #category : #'*metacello-core' }
String >> removeFromMetacelloPackages: aMetacelloPackagesSpec [

	aMetacelloPackagesSpec addMember: 
		(aMetacelloPackagesSpec removeMember 
			name: self;
			yourself)
]

{ #category : #'*metacello-core' }
String >> setAuthorInMetacelloConfig: aMetacelloConfig [

	aMetacelloConfig setAuthorWithString: self
]

{ #category : #'*metacello-core' }
String >> setAuthorInMetacelloVersion: aMetacelloVersionSpec [

	aMetacelloVersionSpec setAuthor: 
		(aMetacelloVersionSpec project valueHolderSpec
			value: self;
			yourself)
]

{ #category : #'*metacello-core' }
String >> setBlessingInMetacelloConfig: aMetacelloConfig [

	aMetacelloConfig setBlessingWithString: self
]

{ #category : #'*metacello-core' }
String >> setBlessingInMetacelloVersion: aMetacelloVersionSpec [

	aMetacelloVersionSpec setBlessing: 
		(aMetacelloVersionSpec project valueHolderSpec
			value: self;
			yourself)
]

{ #category : #'*metacello-core' }
String >> setDescriptionInMetacelloConfig: aMetacelloConfig [

	aMetacelloConfig setDescriptionWithString: self
]

{ #category : #'*metacello-core' }
String >> setDescriptionInMetacelloVersion: aMetacelloVersionSpec [

	aMetacelloVersionSpec setDescription: 
		(aMetacelloVersionSpec project valueHolderSpec
			value: self;
			yourself)
]

{ #category : #'*metacello-core' }
String >> setImportInVersionSpec: aMetacelloVersionSpec [
  aMetacelloVersionSpec setImport: {self}
]

{ #category : #'*metacello-core' }
String >> setIncludesInMetacelloPackage: aMetacelloPackageSpec [

	aMetacelloPackageSpec setIncludes: { self }.
]

{ #category : #'*metacello-core' }
String >> setLoadsInMetacelloProject: aMetacelloPackageSpec [

	aMetacelloPackageSpec setLoads: { self }.
]

{ #category : #'*metacello-core' }
String >> setPackage: aString withInMetacelloConfig: aMetacelloConfig [

	aMetacelloConfig setPackage: aString withString: self
]

{ #category : #'*metacello-core' }
String >> setPostLoadDoItInMetacelloSpec: aMetacelloSpec [
    self asSymbol setPostLoadDoItInMetacelloSpec: aMetacelloSpec
]

{ #category : #'*metacello-core' }
String >> setPreLoadDoItInMetacelloSpec: aMetacelloSpec [
    self asSymbol setPreLoadDoItInMetacelloSpec: aMetacelloSpec
]

{ #category : #'*metacello-core' }
String >> setProject: aString withInMetacelloConfig: aMetacelloConfig [

	aMetacelloConfig setProject: aString withString: self
]

{ #category : #'*metacello-core' }
String >> setRequiresInMetacelloPackage: aMetacelloPackageSpec [

	aMetacelloPackageSpec setRequires: { self }.
]

{ #category : #'*metacello-core' }
String >> setTimestampInMetacelloConfig: aMetacelloConfig [

	aMetacelloConfig setTimestampWithString: self
]

{ #category : #'*metacello-core' }
String >> setTimestampInMetacelloVersion: aMetacelloVersionSpec [

	aMetacelloVersionSpec setTimestamp: 
		(aMetacelloVersionSpec project valueHolderSpec
			value: self;
			yourself)
]
