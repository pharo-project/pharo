"
You can use me to report information on the current image on a Stream.
I provide both system information and information on the image (number of package, LOC, etc.).

I'm used by `StSystemReporter`and `StSizeReporter`.
I provide methods to compute the data or to access it.

"
Class {
	#name : #SystemReporter,
	#superclass : #Object,
	#instVars : [
		'manifestClassLoc',
		'manifestClasses',
		'imageClasses',
		'imageClassPackages',
		'testClasses',
		'testClassPackages',
		'baselineClasses',
		'baselineClassPackages',
		'effectiveClasses',
		'effectiveClassPackages',
		'testClassLoc',
		'baselineClassLoc',
		'imageClassLoc'
	],
	#category : #'System-Reporter'
}

{ #category : #querying }
SystemReporter >> baselineClassLoc [
	^ baselineClassLoc 
		ifNil: [ baselineClassLoc := self linesOfCodeOf: self baselineClasses ]
]

{ #category : #querying }
SystemReporter >> baselineClassPackages [
	^ baselineClassPackages 
		ifNil: [ baselineClassPackages := self baselineClasses collect: [ :each | each package ] as: Set ]
]

{ #category : #querying }
SystemReporter >> baselineClasses [
	^ baselineClasses 
		ifNil: [ baselineClasses := self imageClasses select: [:c | c name beginsWith: 'BaselineOf' ] ]
]

{ #category : #querying }
SystemReporter >> effectiveClassLoc [
	^ self imageClassLoc - self baselineClassLoc - self manifestClassLoc - self testClassLoc
]

{ #category : #querying }
SystemReporter >> effectiveClassPackages [
	^ effectiveClassPackages 
		ifNil: [ effectiveClassPackages := self effectiveClasses collect: [ :each | each package ] as: Set ]
]

{ #category : #querying }
SystemReporter >> effectiveClasses [
	^ effectiveClasses 
		ifNil: [ effectiveClasses := self imageClasses reject: [ :c | c isManifest or: [ c isTestCase or: [ c name beginsWith: 'BaselineOf' ] ]] ]
]

{ #category : #utils }
SystemReporter >> enumerate: aBlock on: aStream [
	self enumerate: aBlock startAt: 0 on: aStream
]

{ #category : #utils }
SystemReporter >> enumerate: aBlock startAt: first on: aStream [
	"Utility method to enumerate Options or Parameters from first to 1000"

	| idx value |
	idx := first.
	[ 
	value := aBlock value: idx.
	value isNil or: [ idx > 1000 ] ]
		whileFalse: [ 
			aStream
				nextPut: $#;
				nextPutAll: idx printString;
				tab;
				nextPutAll: value;
				cr.
			idx := idx + 1 ].
	idx = first
		ifTrue: [ 
			aStream
				nextPutAll: 'none';
				cr ]
]

{ #category : #utils }
SystemReporter >> header: aString on: aStream [
	aStream
		nextPutAll: aString;
		cr.
	aString size timesRepeat: [ aStream nextPut: $- ].
	aStream cr
]

{ #category : #querying }
SystemReporter >> imageClassLoc [
	^ imageClassLoc 
		ifNil: [ imageClassLoc := self linesOfCodeOf: self imageClasses ]
]

{ #category : #querying }
SystemReporter >> imageClassPackages [
	^ imageClassPackages 
		ifNil: [ imageClassPackages := self imageClasses collect: [ :each | each package ] as: Set ]
]

{ #category : #querying }
SystemReporter >> imageClasses [
	^ imageClasses 
		ifNil: [ imageClasses := self class environment allClasses ]
]

{ #category : #utils }
SystemReporter >> linesOfCodeOf: aCollection [
	^ aCollection inject: 0 into: [ :sum :each | sum + each linesOfCode]
]

{ #category : #querying }
SystemReporter >> manifestClassLoc [
	^ manifestClassLoc 
		ifNil: [ manifestClassLoc := self linesOfCodeOf: self manifestClasses ]
]

{ #category : #querying }
SystemReporter >> manifestClasses [
	^ manifestClasses 
		ifNil: [ manifestClasses := self imageClasses select: [ :each | each isManifest ] ]
]

{ #category : #utils }
SystemReporter >> printNumber: aNumber on: aStream [

	aNumber
		printSeparatedBy: Character space 
		every: 3
		signed: false
		on: aStream
]

{ #category : #'building report - system' }
SystemReporter >> readContentsSafelyFromFile: osPath andWriteTo: aStream [

	[ osPath asFileReference 
		readStreamDo: [ :s | aStream nextPutAll: s contents ] ]
		on: Error
		do: [ :ex | ex return: ex printString ]
]

{ #category : #'building report - system' }
SystemReporter >> reportImage: aStream [
	self header: 'Image' on: aStream.
	aStream 
		nextPutAll: Smalltalk image imagePath; cr;
		nextPutAll: SystemVersion current version; cr;
		nextPutAll: Smalltalk image lastUpdateString; cr;
		nextPutAll: ChangeSet current name; cr

]

{ #category : #'building report - system' }
SystemReporter >> reportImageParameters: aStream [
	self header: 'Image Commandline Parameters' on: aStream.
	self enumerate: [:idx | Smalltalk image argumentAt: idx] on: aStream
]

{ #category : #'building report - size' }
SystemReporter >> reportLOC: aStream [
	
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

{ #category : #'building report - system' }
SystemReporter >> reportLinuxOSDetails: aStream [
	self header: 'Operating System Details' on: aStream.
	#(
		'/etc/issue'
		'/etc/lsb-release'
		'/proc/version'
	) do: [:path|
		self readContentsSafelyFromFile: path andWriteTo: aStream]
]

{ #category : #'building report - system' }
SystemReporter >> reportModules: aStream [

	self header: 'Loaded VM Modules' on: aStream.
	Smalltalk vm listLoadedModules asSortedCollection do: [:each | aStream nextPutAll: each; cr].
	aStream cr.
	self header: 'VM Built-in Modules' on: aStream.
	Smalltalk vm listBuiltinModules asSortedCollection do: [:each | aStream nextPutAll: each; cr]


]

{ #category : #'building report - size' }
SystemReporter >> reportNbClasses: aStream [

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

{ #category : #'building report - size' }
SystemReporter >> reportNbTraits: aStream [
	| imageTraits |
	imageTraits := self class environment allTraits.
	self header: 'Number of traits' on: aStream.
	aStream 
		nextPutAll: imageTraits size asString; cr ;
		nextPutAll: 'not used: ' ;
		nextPutAll: (imageTraits count: [ :t | t users isEmpty ]) asString; cr

]

{ #category : #'building report - system' }
SystemReporter >> reportOS: aStream [
	self header: 'Operating System/Hardware' on: aStream.
	aStream
		nextPutAll: Smalltalk os platformName; space;
		nextPutAll: Smalltalk os version; space;
		nextPutAll: Smalltalk os subtype; cr

]

{ #category : #'building report - system' }
SystemReporter >> reportOSEnvironment: aStream [
	| env |
	self header: 'Operating System Environment' on: aStream.
	env := [ Smalltalk os environment] on: Error do: [ ^self ].
	env keys asSortedCollection do: [:name |
		aStream nextPutAll: name; nextPut: $=; nextPutAll: (env at: name); cr ]
]

{ #category : #'building report - system' }
SystemReporter >> reportRepositories: aStream [
	self header: 'Monticello Repositories' on: aStream.
	MCRepositoryGroup default repositories do: [:each | aStream nextPutAll: each description; cr]
]

{ #category : #'building report - size' }
SystemReporter >> reportSmartInfo: aStream [

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

{ #category : #'building report - system' }
SystemReporter >> reportSources: aStream [
	self header: 'Image Sources' on: aStream.
	aStream nextPutAll: SourceFiles class printString; cr.
	aStream nextPutAll: SourceFiles reportString.
]

{ #category : #'building report - system' }
SystemReporter >> reportVM: aStream [
	self header: 'Virtual Machine' on: aStream.
	aStream
		nextPutAll: Smalltalk vm vmFileName;
		cr;
		nextPutAll: Smalltalk vm version;
		cr.
	Smalltalk vm buildDate
		ifNotNil: [ :string | 
			aStream
				nextPutAll: string;
				cr ].
	[ 
	Smalltalk vm platformSourceVersion
		ifNotNil: [ :v | 
			aStream
				nextPutAll: 'platform sources revision ';
				nextPutAll: v;
				cr ] ]
		on: Warning
		do: [ 
			"unsupported primitive"
			 ].
	[ 
	Smalltalk vm interpreterSourceVersion
		ifNotNil: [ :v | 
			aStream
				nextPutAll: 'VMMaker versionString ';
				nextPutAll: v;
				cr ] ]
		on: Warning
		do: [ 
			"unsupported primitive"
			 ].
	[ 
	Smalltalk vm interpreterClass
		ifNotNil: [ :string | 
			aStream
				nextPutAll: string;
				cr ].
	Smalltalk vm cogitClass
		ifNotNil: [ :string | 
			aStream
				nextPutAll: string;
				cr ] ]
		on: Error
		do: [ 
			"unsupported primitives"
			 ]
]

{ #category : #'building report - system' }
SystemReporter >> reportVMOptions: aStream [
	self header: 'Virtual Machine Commandline Options' on: aStream.
	self enumerate: [:idx | Smalltalk vm optionAt: idx] startAt: 1 on: aStream
]

{ #category : #'building report - system' }
SystemReporter >> reportVMParameters: aStream [
	| vmParameters |
	self header: 'Virtual Machine Parameters' on: aStream.
	vmParameters := Smalltalk vm getParameters.
	Smalltalk vm parameterLabels
			 pairsDo: [:idx :desc |
		aStream
			nextPut: $#;
			nextPutAll: idx printString;
			tab;
			nextPutAll: (vmParameters at: idx) printString;
			tab;
			nextPutAll: desc;
			cr]
]

{ #category : #'building report - system' }
SystemReporter >> reportVMStats: aStream [
	self header: 'Virtual Machine Statistics' on: aStream.
	aStream
		nextPutAll: Smalltalk vm statisticsReport
]

{ #category : #'building report - system' }
SystemReporter >> reportWin32GFXDetails: aStream [
	self header: 'Graphics Hardware Details' on: aStream.
	aStream nextPutAll: (Smalltalk vm getSystemAttribute: 10003)
]

{ #category : #'building report - system' }
SystemReporter >> reportWin32HardwareDetails: aStream [
	self header: 'Hardware Details' on: aStream.
	aStream nextPutAll: (Smalltalk vm getSystemAttribute: 10001)
]

{ #category : #'building report - system' }
SystemReporter >> reportWin32OSDetails: aStream [
	self header: 'Operating System Details' on: aStream.
	aStream nextPutAll: (Smalltalk vm getSystemAttribute: 10002)
]

{ #category : #'building report - system' }
SystemReporter >> reportWin32VMConfig: aStream [
	| exePath iniPath |
	self header: 'VM Configuration' on: aStream.
	exePath := Smalltalk vm vmFileName.
	iniPath := (exePath copyUpToLast: $.), '.ini'.
	aStream nextPutAll: iniPath; cr; cr.
	self readContentsSafelyFromFile: iniPath andWriteTo: aStream

]

{ #category : #'building report - system' }
SystemReporter >> reportWorkingCopies: aStream [
	| list |
	self header: 'Monticello Working Copies' on: aStream.
	list := MCWorkingCopy allManagers asSortedCollection: [:a :b | a packageName <= b packageName].
	list do: [:each | aStream nextPutAll: each description; cr ]
]

{ #category : #querying }
SystemReporter >> testClassLoc [
	^ testClassLoc 
		ifNil: [ testClassLoc := self linesOfCodeOf: self manifestClasses ]
]

{ #category : #querying }
SystemReporter >> testClassPackages [
	^ testClassPackages 
		ifNil: [ testClassPackages := self testClasses collect: [ :each | each package ] as: Set ]
]

{ #category : #querying }
SystemReporter >> testClasses [
	^ testClasses 
		ifNil: [ testClasses := self imageClasses select: [ :each | each isTestCase ] ]
]
