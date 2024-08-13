"
A Metacello version
"
Class {
	#name : 'MCVersion',
	#superclass : 'Object',
	#instVars : [
		'package',
		'info',
		'snapshot',
		'dependencies',
		'completeSnapshot'
	],
	#category : 'Monticello-Versioning',
	#package : 'Monticello',
	#tag : 'Versioning'
}

{ #category : 'instance creation' }
MCVersion class >> package: aPackage [
	^ self package: aPackage info: MCVersionInfo new
]

{ #category : 'instance creation' }
MCVersion class >> package: aPackage info: aVersionInfo [
	^ self package: aPackage info: aVersionInfo snapshot: aPackage snapshot
]

{ #category : 'instance creation' }
MCVersion class >> package: aPackage info: aVersionInfo snapshot: aSnapshot [
	^ self package: aPackage info: aVersionInfo snapshot: aSnapshot dependencies: #()
]

{ #category : 'instance creation' }
MCVersion class >> package: aPackage info: aVersionInfo snapshot: aSnapshot dependencies: aCollection [
	^ self new initializeWithPackage: aPackage info: aVersionInfo snapshot: aSnapshot dependencies: aCollection
]

{ #category : 'actions' }
MCVersion >> addToCache [
	MCCacheRepository uniqueInstance storeVersion: self
]

{ #category : 'actions' }
MCVersion >> adopt [
	self workingCopy adopt: self
]

{ #category : 'accessing' }
MCVersion >> changes [
	^ self completeSnapshot patchRelativeToBase: self completePackageSnapshot
]

{ #category : 'accessing' }
MCVersion >> completePackageSnapshot [
	"Answer with a snapshot of current working copy of this package and all its dependencies."

	| definitions |
	definitions := OrderedCollection withAll: package snapshot definitions.
	self dependencies
		do: [ :each | definitions addAll: each package snapshot definitions ]
		displayingProgress: [ :item | 'Loading dependencies from: ' , item package name ].
	^ MCSnapshot fromDefinitions: definitions
]

{ #category : 'accessing' }
MCVersion >> completeSnapshot [
	^ completeSnapshot ifNil: [ completeSnapshot := self loadCompleteSnapshot ]
]

{ #category : 'accessing' }
MCVersion >> dependencies [
	^ dependencies ifNil: [#()]
]

{ #category : 'accessing' }
MCVersion >> fileName [
	^ info name, '.', self writerClass extension
]

{ #category : 'actions' }
MCVersion >> fileOutOn: aStream [
	self writerClass fileOut: self on: aStream
]

{ #category : 'accessing' }
MCVersion >> info [
	^ info
]

{ #category : 'initialization' }
MCVersion >> initializeWithPackage: aPackage info: aVersionInfo snapshot: aSnapshot dependencies: aCollection [
	package := aPackage.
	info := aVersionInfo.
	snapshot := aSnapshot.
	dependencies := aCollection.
	self addToCache.
]

{ #category : 'actions' }
MCVersion >> load [
	MCVersionLoader loadVersion: self
]

{ #category : 'accessing' }
MCVersion >> loadCompleteSnapshot [

	| definitions |
	definitions := OrderedCollection withAll: self snapshot definitions.

	self dependencies
		do: [ :each | definitions addAll: each resolve completeSnapshot definitions ]
		displayingProgress: [ :item | 'Loading dependencies from: ' , item package name ].

	^ MCSnapshot fromDefinitions: definitions
]

{ #category : 'actions' }
MCVersion >> merge [
	MCVersionMerger mergeVersion: self
]

{ #category : 'accessing' }
MCVersion >> package [
	^ package
]

{ #category : 'printing' }
MCVersion >> printOn: aStream [
	super printOn: aStream.
	aStream nextPut: $(; print: self info; nextPut: $).
]

{ #category : 'initialization' }
MCVersion >> setPackage: aPackage info: aVersionInfo snapshot: aSnapshot dependencies: aCollection [
	package := aPackage.
	info := aVersionInfo.
	snapshot := aSnapshot.
	dependencies := aCollection
]

{ #category : 'accessing' }
MCVersion >> snapshot [
	"lazily load snapshot..."
	snapshot isBlock ifTrue: [ 
		snapshot := snapshot value ].
	^ snapshot
]

{ #category : 'accessing' }
MCVersion >> workingCopy [
	^ package workingCopy
]

{ #category : 'accessing' }
MCVersion >> writerClass [
	^ MCMczWriter 
]
