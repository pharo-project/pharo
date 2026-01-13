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
		'packagesSpec'
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

{ #category : 'private' }
MetacelloVersionSpec >> doFetchRequiredFromArray: anArray withEngine: anEngine [

	^ self doWithProgress: [ self fetchRequiredFromArray: anArray withEngine: anEngine ] label: 'Fetching'
]

{ #category : 'private' }
MetacelloVersionSpec >> doLoadRequiredFromArray: anArray withEngine: anEngine [

	^ self doWithProgress: [ self loadRequiredFromArray: anArray withEngine: anEngine ] label: 'Loading'
]

{ #category : 'private' }
MetacelloVersionSpec >> doRecordRequiredFromArray: anArray withEngine: anEngine [

	^ self doWithProgress: [ self recordRequiredFromArray: anArray withEngine: anEngine ] label: 'Recording'
]

{ #category : 'private' }
MetacelloVersionSpec >> doWithProgress: aBlock label: aLabel [

	| displayString result |
	displayString := aLabel , ' baseline of ' , self projectLabel.
	MetacelloNotification signal: displayString , '...'.

	displayString displayProgressFrom: 0 to: 2 during: [ :bar |
			bar value: 1.
			result := aBlock value.
			bar value: 2 ].

	MetacelloNotification signal: '...finished to load the baseline'.
	^ result
]

{ #category : 'private' }
MetacelloVersionSpec >> fetchRequiredFromArray: anArray withEngine: anEngine [

	| directive |
	directive := self recordRequiredFromArray: (anArray ifEmpty: [ self defaultPackageNames ]) withEngine: anEngine.

	directive acceptVisitor: (MetacelloFetchTarget on: anEngine).
	^ directive
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

{ #category : 'private' }
MetacelloVersionSpec >> loadRequiredFromArray: anArray withEngine: anEngine [

	| directive |
	directive := self fetchRequiredFromArray: (anArray ifEmpty: [ self defaultPackageNames ]) withEngine: anEngine.

	directive acceptVisitor: (MetacelloLoadTarget on: anEngine).
	^ directive
]

{ #category : 'accessing' }
MetacelloVersionSpec >> loader: aMetacelloLoadTarget [ 
	
	"nothing"
]

{ #category : 'merging' }
MetacelloVersionSpec >> mergeMap [

	^ super mergeMap
		  at: #preLoadDoIt put: preLoadDoIt;
		  at: #postLoadDoIt put: postLoadDoIt;
		  yourself
]

{ #category : 'merging' }
MetacelloVersionSpec >> mergeSpec: anotherSpec [

	| newSpec |
	newSpec := super mergeSpec: anotherSpec.
	newSpec packagesSpec: (self packagesSpec mergeSpec: anotherSpec packagesSpec).
	newSpec repositories: (self repositories mergeSpec: anotherSpec repositories).
	^ newSpec
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

{ #category : 'private' }
MetacelloVersionSpec >> recordRequiredFromArray: anArray withEngine: anEngine [

	| target |
	target := MetacelloRecordTarget on: anEngine.
	target required: anArray.

	self acceptVisitor: target.

	^ target specBuilder root
]

{ #category : 'actions' }
MetacelloVersionSpec >> recordWithEngine: anEngine [

	^ self doRecordRequiredFromArray: self defaultPackageNames withEngine: anEngine
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
