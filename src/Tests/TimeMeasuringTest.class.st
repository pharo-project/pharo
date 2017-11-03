Class {
	#name : #TimeMeasuringTest,
	#superclass : #TestCase,
	#instVars : [
		'realTime',
		'shouldProfile'
	],
	#category : #Tests-Traits
}

{ #category : #debugging }
TimeMeasuringTest >> debug [
	self resources do: [:res | 
		res isAvailable ifFalse: [^res signalInitializationError]].
	[(self class selector: testSelector) setToDebug; runCase] 
		ensure: [self resources do: [:each | each reset]]
			
]

{ #category : #initialization }
TimeMeasuringTest >> initialize [
	super initialize.
	shouldProfile := false.
]

{ #category : #performance }
TimeMeasuringTest >> measure: measuredBlock [ 
	shouldProfile 
		ifTrue: [Smalltalk tools timeProfiler onBlock: [10 timesRepeat: measuredBlock]].
	realTime := measuredBlock timeToRun
]

{ #category : #debugging }
TimeMeasuringTest >> openDebuggerOnFailingTestMethod [
	shouldProfile := true
]

{ #category : #performance }
TimeMeasuringTest >> reportPerformance [
	| str |
	str := ZnCrPortableWriteStream on: (ZnCharacterWriteStream
			on: (File openForWriteFileNamed: 'performanceReports.txt')
			encoding: 'utf8').
	
	str setToEnd;
		nextPutAll: ' test: ', testSelector;
		nextPutAll: ' time: ', realTime asString; 
		nextPutAll: ' version: ', self versionInformation;
		cr; 
		close
]

{ #category : #running }
TimeMeasuringTest >> runCase [
	[super runCase] ensure: [self reportPerformance]
]

{ #category : #debugging }
TimeMeasuringTest >> setToDebug [
	shouldProfile := true

]

{ #category : #utilities }
TimeMeasuringTest >> versionInfoForWorkingCopiesThat: wcPredicate [ 
	^(MCWorkingCopy allManagers select: wcPredicate) inject: ''
		into: [:s :e | s , e description]
]

{ #category : #accessing }
TimeMeasuringTest >> versionInformation [
	| wcPredicate |
	wcPredicate := self workingCopyPredicate.
	^self versionInfoForWorkingCopiesThat: wcPredicate
]

{ #category : #accessing }
TimeMeasuringTest >> workingCopyPredicate [
	^[:e | '*Traits*' match: e package name]
]
