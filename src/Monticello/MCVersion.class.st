"
A Metacello version
"
Class {
	#name : #MCVersion,
	#superclass : #Object,
	#instVars : [
		'package',
		'info',
		'snapshot',
		'dependencies',
		'completeSnapshot'
	],
	#category : 'Monticello-Versioning'
}

{ #category : #'instance creation' }
MCVersion class >> package: aPackage [
	^ self package: aPackage info: MCVersionInfo new
]

{ #category : #'instance creation' }
MCVersion class >> package: aPackage info: aVersionInfo [
	^ self package: aPackage info: aVersionInfo snapshot: aPackage snapshot
]

{ #category : #'instance creation' }
MCVersion class >> package: aPackage info: aVersionInfo snapshot: aSnapshot [
	^ self package: aPackage info: aVersionInfo snapshot: aSnapshot dependencies: #()
]

{ #category : #'instance creation' }
MCVersion class >> package: aPackage info: aVersionInfo snapshot: aSnapshot dependencies: aCollection [
	^ self new initializeWithPackage: aPackage info: aVersionInfo snapshot: aSnapshot dependencies: aCollection
]

{ #category : #actions }
MCVersion >> addToCache [
	MCCacheRepository uniqueInstance storeVersion: self
]

{ #category : #actions }
MCVersion >> adopt [
	self workingCopy adopt: self
]

{ #category : #enumerating }
MCVersion >> allAvailableDependenciesDo: aBlock [
	
	self dependencies do:
		[:ea |
		[ | version |version := ea resolve.
		version allAvailableDependenciesDo: aBlock.
		aBlock value: version]
			on: Error do: []]
]

{ #category : #enumerating }
MCVersion >> allDependenciesDo: aBlock [
	self allDependenciesDo: aBlock ifUnresolved: [:ignored | true]
]

{ #category : #enumerating }
MCVersion >> allDependenciesDo: aBlock ifUnresolved: failBlock [
	| dict |
	dict := Dictionary new.
	self allDependenciesNotIn: dict do: aBlock ifUnresolved: failBlock
]

{ #category : #enumerating }
MCVersion >> allDependenciesNotIn: aDictionary do: aBlock ifUnresolved: failBlock [
	
	self dependencies do: 
		[:ea | | version | 
		version := aDictionary at: ea ifAbsent: [ea resolve].
		version 
			ifNil: [failBlock value: ea]
			ifNotNil: [(aDictionary includes: version) ifFalse:
						[aDictionary at: ea put: version.
						version 
							allDependenciesNotIn: aDictionary 
							do: aBlock
							ifUnresolved: failBlock.
						aBlock value: version]]]
]

{ #category : #converting }
MCVersion >> asDiffAgainst: aVersion [
	aVersion info = self info ifTrue: [self error: 'Cannot diff against self!'].
	^ MCDiffyVersion
		package: self package
		info: self info
		snapshot: self snapshot
		dependencies: self dependencies
		baseVersion: aVersion
]

{ #category : #testing }
MCVersion >> canOptimizeLoading [
	"Answer wether I can provide a patch for the working copy without the usual diff pass"
	^false
]

{ #category : #accessing }
MCVersion >> changes [
	^ self completeSnapshot patchRelativeToBase: self completePackageSnapshot
]

{ #category : #accessing }
MCVersion >> completePackageSnapshot [
	"Answer with a snapshot of current working copy of this package and all its dependencies."
	^self loadCompletePackageSnapshot
]

{ #category : #accessing }
MCVersion >> completeSnapshot [
	^ completeSnapshot ifNil: [ completeSnapshot := self loadCompleteSnapshot ]
]

{ #category : #accessing }
MCVersion >> dependencies [
	^ dependencies ifNil: [#()]
]

{ #category : #accessing }
MCVersion >> fileName [
	^ info name, '.', self writerClass extension
]

{ #category : #actions }
MCVersion >> fileOutOn: aStream [
	self writerClass fileOut: self on: aStream
]

{ #category : #accessing }
MCVersion >> info [
	^ info
]

{ #category : #initialization }
MCVersion >> initializeWithPackage: aPackage info: aVersionInfo snapshot: aSnapshot dependencies: aCollection [
	package := aPackage.
	info := aVersionInfo.
	snapshot := aSnapshot.
	dependencies := aCollection.
	self addToCache.
]

{ #category : #testing }
MCVersion >> isCacheable [
	^true
]

{ #category : #testing }
MCVersion >> isDiffy [
	^ false
]

{ #category : #actions }
MCVersion >> load [
	MCVersionLoader loadVersion: self
]

{ #category : #accessing }
MCVersion >> loadCompletePackageSnapshot [
	| definitions |
	definitions := OrderedCollection withAll: package snapshot definitions.
	
	self dependencies 
		do: [ :each| definitions addAll: each package snapshot definitions ]
		displayingProgress: [ :item| 'Loading dependencies from: ', item package name ].
	
	^ MCSnapshot fromDefinitions: definitions
	
	
]

{ #category : #accessing }
MCVersion >> loadCompleteSnapshot [
	|definitions|
	
	definitions := OrderedCollection withAll: self snapshot definitions.
	
	self dependencies 
		do: [ :each| definitions addAll: each resolve completeSnapshot definitions ]
		displayingProgress: [ :item| 'Loading dependencies from: ', item package name ].
	
	^ MCSnapshot fromDefinitions: definitions
]

{ #category : #actions }
MCVersion >> merge [
	MCVersionMerger mergeVersion: self
]

{ #category : #accessing }
MCVersion >> package [
	^ package
]

{ #category : #printing }
MCVersion >> printOn: aStream [
	super printOn: aStream.
	aStream nextPut: $(.
	aStream nextPutAll: self info printString.
	aStream nextPut: $).
]

{ #category : #initialization }
MCVersion >> setPackage: aPackage info: aVersionInfo snapshot: aSnapshot dependencies: aCollection [
	package := aPackage.
	info := aVersionInfo.
	snapshot := aSnapshot.
	dependencies := aCollection
]

{ #category : #accessing }
MCVersion >> snapshot [
	"lazily load snapshot..."
	snapshot isBlock ifTrue: [ 
		snapshot := snapshot value ].
	^ snapshot
]

{ #category : #accessing }
MCVersion >> summary [
	^ String streamContents:
		[:s |
		s nextPutAll: info summaryHeader.
		(dependencies isNil or: [dependencies isEmpty]) ifFalse:
			[s cr; nextPutAll: 'Dependencies: '.
			dependencies
				do: [:ea | s nextPutAll: ea versionInfo name]
				separatedBy: [s nextPutAll: ', ']].
		s cr; cr; nextPutAll: info message]
]

{ #category : #enumerating }
MCVersion >> withAllDependenciesDo: aBlock [
	self allDependenciesDo: aBlock ifUnresolved: [:ignored |].
	aBlock value: self
]

{ #category : #enumerating }
MCVersion >> withAllDependenciesDo: aBlock ifUnresolved: failBlock [
	| dict |
	dict := Dictionary new.
	self allDependenciesNotIn: dict do: aBlock ifUnresolved: failBlock.
	aBlock value: self
]

{ #category : #accessing }
MCVersion >> workingCopy [
	^ package workingCopy
]

{ #category : #accessing }
MCVersion >> writerClass [
	^ MCMczWriter 
]
