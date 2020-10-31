"
I'm a reporter dedicated to size of the system.

```
StSizeReporter new openWithSpec
```

"
Class {
	#name : #StSizeReporter,
	#superclass : #StReporter,
	#instVars : [
		'data'
	],
	#category : #'NewTools-SystemReporter'
}

{ #category : #accessing }
StSizeReporter >> baselineClassLoc [
	^ data at: #baselineClassLoc ifAbsent: [ self computeLocs. data at: #baselineClassLoc ]
]

{ #category : #accessing }
StSizeReporter >> baselineClasses [

	^ data at: #baselineClasses
]

{ #category : #compute }
StSizeReporter >> computeLocs [ 

	data at: #manifestClassLoc put:  (self manifestClasses inject: 0 into: [ :sum :each | sum + each linesOfCode]).
	data at: #testClassLoc put: (self testClasses inject: 0 into: [ :sum :each | sum + each linesOfCode]).
	data at: #baselineClassLoc put: (self baselineClasses inject: 0 into: [ :sum :each | sum + each linesOfCode]).
	data at: #imageClassLoc put: (self imageClasses inject: 0 into: [ :sum :each | sum + each linesOfCode]).
	data at: #effectiveClassLoc put: self imageClassLoc - self baselineClassLoc - self manifestClassLoc - self testClassLoc
]

{ #category : #accessing }
StSizeReporter >> effectiveClassLoc [

	^ data at: #effectiveClassLoc ifAbsentPut: [ self computeLocs. 
			self imageClassLoc - self baselineClassLoc - self testClassLoc - self manifestClassLoc ]
	
]

{ #category : #accessing }
StSizeReporter >> effectiveClassPackages [

	^ data at: #effectiveClassPackages
]

{ #category : #accessing }
StSizeReporter >> effectiveClasses [

	^ data at: #effectiveClasses
]

{ #category : #'building report' }
StSizeReporter >> extent [

	^ 600@ 400
]

{ #category : #initialization }
StSizeReporter >> fillTopics [

  {	#'# smart report' -> #reportSmartInfo: .
	#'# raw classes' -> #reportNbClasses:  .
	#'# raw traits' -> #reportNbTraits: .
	#'# raw LOC' -> #reportLOC: }
		do: [ :each | self addTopicSpec: each ].

		 
]

{ #category : #accessing }
StSizeReporter >> imageClassLoc [ 
	^ data at: #imageClassLoc  ifAbsent: [ self computeLocs. data at: #imageClassLoc ]
]

{ #category : #accessing }
StSizeReporter >> imageClassPackages [

	^ data at: #imageClassPackages
]

{ #category : #accessing }
StSizeReporter >> imageClasses [

	^ data at: #imageClasses
]

{ #category : #initialization }
StSizeReporter >> initialize [ 
	super initialize.
	data := Dictionary new. 
	data at: #imageClasses put: self class environment allClasses.
	data at: #imageClassPackages put: (self imageClasses collect: [ :each | each package ] as: Set).
	
	data at: #manifestClasses put: (self imageClasses select: [ :each | each isManifest ]).
	data at: #manifestClassPackages put: (self manifestClasses collect: [ :each | each package ] as: Set).
	
	data at: #testClasses put: (self imageClasses select: [ :each | each isTestCase ]).
	data at: #testClassPackages put: (self testClasses collect: [ :each | each package ] as: Set).
	
	data at: #baselineClasses put: (self imageClasses select: [:c | c name beginsWith: 'BaselineOf' ]).
	data at: #baselineClassPackages put: (self baselineClasses collect: [ :each | each package ] as: Set).
	
	data at: #effectiveClasses put: (self imageClasses reject: [ :c | c isManifest or: [ c isTestCase or: [ c name beginsWith: 'BaselineOf' ] ]]).
	data at: #effectiveClassPackages put: (self effectiveClasses collect: [ :each | each package ] as: Set).
	
	topicList selectIndex: 1.
]

{ #category : #accessing }
StSizeReporter >> manifestClassLoc [

	^ data at: #manifestClassLoc ifAbsent: [ self computeLocs. data at: #manifestClassLoc ]
	
]

{ #category : #accessing }
StSizeReporter >> manifestClasses [

	^ data at: #manifestClasses
]

{ #category : #utils }
StSizeReporter >> printNumber: aNumber on: aStream [

	aNumber
		printSeparatedBy: Character space 
		every: 3
		signed: false
		on: aStream
]

{ #category : #'building report' }
StSizeReporter >> reportLOC: aStream [
	
	self header: 'Number of LinesOfCode' on: aStream.
	aStream nextPutAll: '# for all classes: '.
	self printNumber: self imageClassLoc on: aStream.
	aStream cr.
	aStream nextPutAll: '# for manifest classes: '.
	self printNumber: self manifestClassLoc on: aStream.
	aStream cr.
	aStream nextPutAll: '# for test classes: '.
	self printNumber: self testClassLoc on: aStream.
	aStream cr.
	aStream nextPutAll: '# for baselines: '.
	self printNumber: self baselineClassLoc on: aStream.
	aStream cr.
	aStream nextPutAll: '# for effective classes: '.
	self printNumber: self effectiveClassLoc on: aStream.
	aStream cr.
	aStream nextPutAll: 'Average LOC per class for effective classes: '.
	aStream print: (self effectiveClassLoc / self effectiveClasses size) asFloat rounded
]

{ #category : #'building report' }
StSizeReporter >> reportNbClasses: aStream [

	self header: 'Number of classes' on: aStream.
	aStream nextPutAll: '# All classes '.
	self printNumber: self imageClasses size on: aStream.
	aStream cr; nextPutAll: '# Tests classes: '.
	self printNumber: self testClasses size on: aStream.
	aStream cr; nextPutAll: '# Manifest classes: '.
	self printNumber: self manifestClasses size on: aStream.
	aStream cr; nextPutAll: '# BaselineOf-s:  '.
	self printNumber: self baselineClasses size on: aStream.
	aStream cr; nextPutAll: '# Effective classes:  '.
	self printNumber: (self imageClasses size - self testClasses size - self manifestClasses size - self baselineClasses size) on: aStream.
		
		
]

{ #category : #'building report' }
StSizeReporter >> reportNbTraits: aStream [
	| imageTraits |
	imageTraits := self class environment allTraits.
	self header: 'Number of traits' on: aStream.
	aStream 
		nextPutAll: imageTraits size asString; cr ;
		nextPutAll: 'not used: ' ;
		nextPutAll: (imageTraits count: [ :t | t users isEmpty ]) asString; cr

]

{ #category : #'building report' }
StSizeReporter >> reportSmartInfo: aStream [

	self header: 'Smarter data' on: aStream.
	aStream nextPutAll: 'Analysis foreffective classes'.
	aStream cr.
	aStream nextPutAll: ' i.e., no test, baseline, manifest classes'.
	aStream cr.
	aStream nextPutAll: '# packages: '.
	self printNumber: self effectiveClassPackages size on: aStream.
	aStream cr.
	aStream nextPutAll: '# classes: '.
	self printNumber: self effectiveClasses size on: aStream.
	aStream cr.
	aStream nextPutAll: '# average number of classes per package: '.
	aStream print: (self effectiveClasses size / self effectiveClassPackages size) asFloat rounded.
	aStream cr.
	aStream nextPutAll: '# max number of classes in a package: '.
	aStream print: (self effectiveClassPackages asOrderedCollection max: [ :aPackage | aPackage definedClasses size]).
	aStream cr.
	aStream nextPutAll: '# average number of methods per classes: '.
	aStream print: ((self effectiveClasses inject: 0 into: [ :sum :c | sum + c numberOfMethods ]) / self effectiveClasses size) asFloat rounded.
	


]

{ #category : #accessing }
StSizeReporter >> testClassLoc [

	^ data at: #testClassLoc ifAbsent: [ self computeLocs. data at: #testClassLoc ]
	
]

{ #category : #accessing }
StSizeReporter >> testClasses [

	^ data at: #testClasses
]

{ #category : #ui }
StSizeReporter >> title [ 

	^ 'Size reporter'
]

{ #category : #'building report' }
StSizeReporter >> topicSpec [
  {	#'# smart report' -> #reportSmartInfo: .
	#'# raw classes' -> #reportNbClasses:  .
	#'# raw traits' -> #reportNbTraits: .
	#'# raw LOC' -> #reportLOC: }
		do: [ :each | self addTopicSpec: each ].
	^ topics
		 
]
