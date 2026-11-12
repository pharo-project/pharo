Class {
	#name : 'MetacelloVersionSpec',
	#superclass : 'MetacelloSpec',
	#instVars : [
		'project',
		'repositories',
		'importArray',
		'postLoadDoIt',
		'importName',
		'preLoadDoIt',
		'packagesSpec',
		'strict'
	],
	#category : 'Metacello-Core-Model',
	#package : 'Metacello-Core',
	#tag : 'Model'
}

{ #category : 'class initialization' }
MetacelloVersionSpec class >> initialize [
	"Added in Pharo 14."

	self deprecatedAliases: #( MetacelloVersion )
]

{ #category : 'instance creation' }
MetacelloVersionSpec class >> project: aMetacelloProject [

	^ self new
		  project: aMetacelloProject;
		  yourself
]

{ #category : 'visiting' }
MetacelloVersionSpec >> acceptVisitor: aVisitor [
	
	^ aVisitor visitVersionSpec: self
]

{ #category : 'enumerating' }
MetacelloVersionSpec >> currentlyLoadedClassesInVersion [

	| classes |
	classes := Set new.
	self packages do: [ :packageSpec | self class packageOrganizer packageNamed: packageSpec name ifPresent: [ :package | classes addAll: package classes ] ].
	^ classes
]

{ #category : 'loading' }
MetacelloVersionSpec >> defaultPackageNames [
	"if there is a package named 'default' (a group) then it defines the default package names,
	 otherwise answer a list of all of the package names in this version"

	self packagesSpec loadableSpecNamed: 'default' ifAbsent: [ ^ self packageNames ].
	^ #( 'default' )
]

{ #category : 'accessing' }
MetacelloVersionSpec >> groups [
	"Answers the list of groups associated with this version"

	^ self packageSpecsInLoadOrder select: [ :spec | spec isGroupSpec ]
]

{ #category : 'testing' }
MetacelloVersionSpec >> hasImports [

	^ self import isNotNil or: [ self importArray isNotNil ]
]

{ #category : 'accessing' }
MetacelloVersionSpec >> import [
    ^ importName
]

{ #category : 'accessing' }
MetacelloVersionSpec >> import: aString [

	importName := aString
]

{ #category : 'accessing' }
MetacelloVersionSpec >> import: aString provides: anArray [

	importArray := importArray
		               ifNil: [ { (aString -> anArray) } ]
		               ifNotNil: [ importArray , { (aString -> anArray) } ]
]

{ #category : 'accessing' }
MetacelloVersionSpec >> importArray [

	^ importArray
]

{ #category : 'printing' }
MetacelloVersionSpec >> label [

	^ 'baseline [' , self projectLabel , ']'
]

{ #category : 'querying' }
MetacelloVersionSpec >> loadableSpecsFor: aCollectionOfNames [
	"The loadable specs (packages and projects, not groups) that loading aCollectionOfNames reaches through includes and requires"

	| loadableSpecs requirementList managedRequirements |
	loadableSpecs := OrderedCollection new.
	requirementList := aCollectionOfNames copy.
	managedRequirements := Set new.
	[ requirementList isEmpty ] whileFalse: [
			| newReqd |
			newReqd := Set new.
			requirementList do: [ :req |
					| loadableSpec |
					loadableSpec := self packageNamed: req ifAbsent: [ ^ self error: 'Name not found: ' , req , ' in ' , self label ].
					loadableSpec resolveToLoadableSpec ifNotNil: [ :resolvedSpec | loadableSpecs add: resolvedSpec ].
					newReqd addAll: loadableSpec requires.
					newReqd addAll: loadableSpec includes ].
			managedRequirements addAll: requirementList.
			newReqd removeAllFoundIn: managedRequirements.
			requirementList := newReqd ].
	^ loadableSpecs
]

{ #category : 'accessing' }
MetacelloVersionSpec >> loader: aMetacelloLoadTarget [ 
	
	"nothing"
]

{ #category : 'querying' }
MetacelloVersionSpec >> packageNamed: aString [

	^ self packageNamed: aString ifAbsent: [ nil ]
]

{ #category : 'querying' }
MetacelloVersionSpec >> packageNamed: aString ifAbsent: absentBlock [

	self packagesSpec loadableSpecNamed: aString ifPresent: [ :spec | ^ spec ].

	self hasImports ifTrue: [ "expect the 'missing' name to be satisfied within context of imported project"
			importArray ifNotNil: [
					importArray do: [ :assoc |
							(assoc value includes: aString) ifTrue: [
									self packagesSpec loadableSpecNamed: assoc key ifPresent: [ :spec |
											^ spec
												  mergeImportLoads: { aString };
												  yourself ] ] ] ].
			self import ifNotNil: [
					^ (self packagesSpec loadableSpecNamed: self import ifAbsent: absentBlock)
						  mergeImportLoads: { aString };
						  yourself ] ].

	^ (#( 'default' 'ALL' ) includes: aString)
		  ifTrue: [
				  self project groupSpec
					  name: aString;
					  includes: self packageNames;
					  yourself ]
		  ifFalse: [ absentBlock value ]
]

{ #category : 'querying' }
MetacelloVersionSpec >> packageNames [

	^ self packagesSpec loadableSpecs collect: [ :spec | spec name ]
]

{ #category : 'querying' }
MetacelloVersionSpec >> packageSpecsInLoadOrder [

	^ self packagesSpec packageSpecsInLoadOrderFor: self
]

{ #category : 'querying' }
MetacelloVersionSpec >> packageSpecsInLoadOrderFor: aCollectionOfNames [
	"The package and project specs to load for aCollectionOfNames, in load order, imports included"

	| loadOrder specsToLoad packageNames importNames importSpec importProjectSpecs importProjectNameMap |
	loadOrder := self packageSpecsInLoadOrder.
	importNames := (packageNames := ((self loadableSpecsFor: aCollectionOfNames) collect: [ :spec | spec name ]) asSet) copy.

	self hasImports ifFalse: [ ^ loadOrder select: [ :spec | packageNames includes: spec name ] ].

	loadOrder do: [ :spec | importNames remove: spec name ifAbsent: [ ] ].
	specsToLoad := OrderedCollection new.
	importProjectSpecs := Dictionary new.
	importProjectNameMap := Dictionary new.
	self importArray ifNotNil: [ :imports |
			loadOrder do: [ :spec |
					imports do: [ :assoc |
							assoc key = spec name ifTrue: [
									importProjectSpecs at: spec name put: spec.
									(assoc value select: [ :each | importNames includes: each ]) do: [ :each |
										(importProjectNameMap at: spec name ifAbsentPut: [ Set new ]) add: each ] ] ] ] ].

	self import ifNotNil: [ :importedProjectName |
			loadOrder do: [ :spec |
					spec name = importedProjectName ifTrue: [
							importProjectSpecs at: spec name put: spec.
							importProjectNameMap at: spec name put: importNames ] ] ].

	loadOrder do: [ :spec |
			(packageNames includes: spec name) ifTrue: [ specsToLoad add: spec ].
			importProjectSpecs at: spec name ifPresent: [ :importProjectSpec | "insert the imports at this point"
					importProjectNameMap at: spec name ifPresent: [ :importedNames |
							importedNames do: [ :importedName |
									specsToLoad add: (importSpec := importProjectSpec copy
											               name: importedName;
											               mergeImportLoads: { importedName };
											               yourself).
									importSpec projectReference name: importedName ] ] ] ].
	^ specsToLoad
]

{ #category : 'accessing' }
MetacelloVersionSpec >> packages [
	"Answers the list of packages associated with this version"

	^ self packageSpecsInLoadOrder select: [ :spec | spec isPackageSpec ]
]

{ #category : 'accessing' }
MetacelloVersionSpec >> packagesSpec [

	^ packagesSpec ifNil: [ packagesSpec := MetacelloPackagesSpec new ]
]

{ #category : 'accessing' }
MetacelloVersionSpec >> packagesSpec: anObject [

	packagesSpec := anObject
]

{ #category : 'copying' }
MetacelloVersionSpec >> postCopy [

	super postCopy.
	packagesSpec := packagesSpec copy.
	repositories := repositories copy
]

{ #category : 'querying' }
MetacelloVersionSpec >> postLoadDoIt [

	^postLoadDoIt
]

{ #category : 'accessing' }
MetacelloVersionSpec >> postLoadDoIt: aSymbol [

	self validateDoItSelector: aSymbol.
	postLoadDoIt := aSymbol
]

{ #category : 'querying' }
MetacelloVersionSpec >> preLoadDoIt [

	^preLoadDoIt
]

{ #category : 'accessing' }
MetacelloVersionSpec >> preLoadDoIt: aSymbol [

	self validateDoItSelector: aSymbol.
	preLoadDoIt := aSymbol
]

{ #category : 'printing' }
MetacelloVersionSpec >> printOn: aStream [

	super printOn: aStream.
	aStream
		nextPutAll: ' [';
		nextPutAll: self project label;
		nextPutAll: ']'
]

{ #category : 'accessing' }
MetacelloVersionSpec >> project [

	^ project
]

{ #category : 'accessing' }
MetacelloVersionSpec >> project: aMetacelloProject [

	project := aMetacelloProject
]

{ #category : 'accessing' }
MetacelloVersionSpec >> projectLabel [

	^self project label
]

{ #category : 'accessing' }
MetacelloVersionSpec >> projects [
	"Answers the list of projects associated with this version"

	^ self packageSpecsInLoadOrder
		  select: [ :spec | spec isProjectReferenceSpec ]
		  thenCollect: [ :spec | spec projectReference ]
]

{ #category : 'accessing' }
MetacelloVersionSpec >> repositories [

	^ repositories ifNil: [ repositories := MetacelloRepositoriesSpec new ]
]

{ #category : 'accessing' }
MetacelloVersionSpec >> repositories: anObject [
	repositories := anObject
]

{ #category : 'accessing' }
MetacelloVersionSpec >> repository: aString [

	self repositories add: aString
]

{ #category : 'accessing' }
MetacelloVersionSpec >> repository: aString username: username password: password [

	self repositories repository: aString username: username password: password
]

{ #category : 'loading' }
MetacelloVersionSpec >> repositorySpecs [

	^ self repositories repositorySpecs
]

{ #category : 'accessing' }
MetacelloVersionSpec >> spec [
	"To remove once Iceberg is fixed."

	^ self
]

{ #category : 'accessing' }
MetacelloVersionSpec >> strict [

	^ strict
]

{ #category : 'accessing' }
MetacelloVersionSpec >> strict: aBoolean [

	strict := aBoolean
]
