"
CircularMemoryLogger records the signals that it receives in a circular buffer, default size 5000 entries.

!!Example1: Instance usage
[[[
	(CircularMemoryLogger new 
		runDuring: [ 
			StringSignal emit: 'This is a message' ]	)
				inspect.
]]]

!!Example 2: Global usage
[[[
	CircularMemoryLogger reset.
	CircularMemoryLogger start.
	StringSignal emit: 'This is a message' .
	CircularMemoryLogger instance recordings inspect.
	CircularMemoryLogger stop.
]]]
"
Class {
	#name : #CircularMemoryLogger,
	#superclass : #MemoryLogger,
	#instVars : [
		'entryCount',
		'index',
		'fullBuffer',
		'loopCount'
	],
	#category : #'Beacon-Core-Loggers'
}

{ #category : #'instance creation' }
CircularMemoryLogger class >> fromSton: stonReader [

	| logger |

	logger := self basicNew.
	stonReader parseListDo: [ :each :idx |
		idx = 1 ifFalse: [ self error: 'Invalid STON' ].
		logger
			name: (each at: #name);
			basicRecordings: (each at: #recordings);
			basicEntryCount: (each at: #entryCount);
			basicIndex: (each at: #index);
			basicFullBuffer: (each at: #fullBuffer);
			basicLoopCount: (each at: #loopCount) ].

	^logger
]

{ #category : #'ston persistence' }
CircularMemoryLogger >> asStonDictionary [

	^Dictionary new
		at: #name put: name;
		at: #recordings put: recordings;
		at: #entryCount put: entryCount;
		at: #index put: index;
		at: #fullBuffer put: fullBuffer;
		at: #loopCount put: loopCount;
		yourself.
]

{ #category : #'ston persistence' }
CircularMemoryLogger >> basicEntryCount: anInteger [
	"Set the receiver's entryCount.
	This is normally only used when loading saved instances from file"

	entryCount := anInteger
]

{ #category : #'ston persistence' }
CircularMemoryLogger >> basicFullBuffer: aBoolean [
	"Set the receiver's fullBuffer.
	This is normally only used when loading saved instances from file"

	fullBuffer := aBoolean
]

{ #category : #'ston persistence' }
CircularMemoryLogger >> basicIndex: anInteger [
	"Set the receiver's index.
	This is normally only used when loading saved instances from file"

	index := anInteger
]

{ #category : #'ston persistence' }
CircularMemoryLogger >> basicLoopCount: anInteger [
	"Set the receiver's fullBuffer.
	This is normally only used when loading saved instances from file"

	loopCount := anInteger
]

{ #category : #initialize }
CircularMemoryLogger >> basicReset [
	"it simply removes all recordings without
	affecting the running status of the logger.
	This method must be called from within the mutex."

	recordings := Array new: entryCount.
	fullBuffer := false.
	loopCount := 0.
	index := 0.
]

{ #category : #initialize }
CircularMemoryLogger >> entryCount: anInteger [

	mutex critical: [
		entryCount := anInteger.
		self basicReset ]
]

{ #category : #initialize }
CircularMemoryLogger >> initialize [ 

	entryCount := 5000.
	index := 0.
	super initialize.

]

{ #category : #accessing }
CircularMemoryLogger >> nextPut: aSignal [
	mutex critical: [
		index := index >= entryCount ifTrue: [ 
			fullBuffer := true.
			loopCount := loopCount + 1.
			1 ]
		ifFalse: 
			[ index + 1 ].
		recordings at: index put: aSignal ].
	"the announcement is passed forward so that potential viewers
	can refresh when something changes. we do not create another announcement
	because it is not needed given that the only thing that can happen is
	for a new announcement to be recorded"
	self announcer announce: aSignal
]

{ #category : #accessing }
CircularMemoryLogger >> recordings [
	"It answers a copy of the recorded collection to not grant
	access to the users to a shared resource that should only be 
	modified via the mutex"
	^ (fullBuffer
			ifTrue: [ recordings sorted: [ :a :b | a timestamp < b timestamp ] ]
			ifFalse: [ recordings first: index ])
]

{ #category : #initialize }
CircularMemoryLogger >> reset [
	"it simply removes all recordings without
	affecting the running status of the logger"
	mutex critical: [ self basicReset ]
]
