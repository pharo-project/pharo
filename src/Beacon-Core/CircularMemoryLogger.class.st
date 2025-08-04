"
`CircularMemoryLogger` records the signals that it receives in a circular buffer, default size 5000 entries.

###Example1: Instance usage
The following example illustrates how a logger can be used within a giving scope, here during the block execution. 
```
	(CircularMemoryLogger new 
		runDuring: [ 
			StringSignal emit: 'This is a message' ]	)
				inspect.
```

###Example 2: Global usage
The following example shows how the logger can be used globally. 

```
	CircularMemoryLogger reset.
	CircularMemoryLogger start.
	StringSignal emit: 'This is a message' .
	CircularMemoryLogger instance recordings inspect.
	CircularMemoryLogger stop.
```
"
Class {
	#name : 'CircularMemoryLogger',
	#superclass : 'MemoryLogger',
	#instVars : [
		'fullBuffer',
		'loopCount',
		'maximumEntryCount',
		'lastEntryIndex'
	],
	#category : 'Beacon-Core-Loggers',
	#package : 'Beacon-Core',
	#tag : 'Loggers'
}

{ #category : 'instance creation' }
CircularMemoryLogger class >> fromSton: stonReader [

	| logger |
	logger := self basicNew.
	stonReader parseListDo: [ :each :idx |
		idx = 1 ifFalse: [ self error: 'Invalid STON' ].
		logger
			name: (each at: #name);
			basicEntries: (each at: #entries);
			basicEntryCount: (each at: #entryCount);
			basicIndex: (each at: #lastEntryIndex);
			basicFullBuffer: (each at: #fullBuffer);
			basicLoopCount: (each at: #loopCount) ].

	^ logger
]

{ #category : 'ston persistence' }
CircularMemoryLogger >> asStonDictionary [

	^ Dictionary new
		  at: #name put: name;
		  at: #entries put: entries;
		  at: #maximumEntryCount put: maximumEntryCount;
		  at: #index put: lastEntryIndex;
		  at: #fullBuffer put: fullBuffer;
		  at: #loopCount put: loopCount;
		  yourself
]

{ #category : 'ston persistence' }
CircularMemoryLogger >> basicEntries: aCollection [
	"Set the receiver's recordings.
	This is normally only used when loading saved instances from file"

	entries := aCollection
]

{ #category : 'ston persistence' }
CircularMemoryLogger >> basicEntryCount: anInteger [
	"Set the receiver's entryCount.
	This is normally only used when loading saved instances from file"

	maximumEntryCount := anInteger
]

{ #category : 'ston persistence' }
CircularMemoryLogger >> basicFullBuffer: aBoolean [
	"Set the receiver's fullBuffer.
	This is normally only used when loading saved instances from file"

	fullBuffer := aBoolean
]

{ #category : 'ston persistence' }
CircularMemoryLogger >> basicIndex: anInteger [
	"Set the receiver's index.
	This is normally only used when loading saved instances from file"

	lastEntryIndex := anInteger
]

{ #category : 'ston persistence' }
CircularMemoryLogger >> basicLoopCount: anInteger [
	"Set the receiver's fullBuffer.
	This is normally only used when loading saved instances from file"

	loopCount := anInteger
]

{ #category : 'ston persistence' }
CircularMemoryLogger >> basicRecordings: aCollection [
	"Set the receiver's recordings.
	This is normally only used when loading saved instances from file"

	entries := aCollection
]

{ #category : 'initialization' }
CircularMemoryLogger >> basicReset [
	"It simply removes all recordings without
	affecting the running status of the logger.
	This method must be called from within the mutex."

	entries := Array new: maximumEntryCount.
	fullBuffer := false.
	loopCount := 0.
	lastEntryIndex := 0
]

{ #category : 'accessing' }
CircularMemoryLogger >> entries [
	"It answers a copy of the recorded collection to not grant
	access to the users to a shared resource that should only be
	modified via the mutex"

	^ fullBuffer
		  ifTrue: [ entries sorted: [ :a :b | a timestamp < b timestamp ] ]
		  ifFalse: [ entries first: lastEntryIndex ]
]

{ #category : 'initialization' }
CircularMemoryLogger >> entryCount: anInteger [

	mutex critical: [
		maximumEntryCount := anInteger.
		self basicReset ]
]

{ #category : 'initialization' }
CircularMemoryLogger >> initialize [

	maximumEntryCount := 5000.
	lastEntryIndex := 0.
	super initialize
]

{ #category : 'logging' }
CircularMemoryLogger >> nextPut: aSignal [

	mutex critical: [
		lastEntryIndex := lastEntryIndex >= maximumEntryCount
			         ifTrue: [
				         fullBuffer := true.
				         loopCount := loopCount + 1.
				         1 ]
			         ifFalse: [ lastEntryIndex + 1 ].
		entries at: lastEntryIndex put: aSignal ].

	"The announcement is passed forward so that potential viewers
	can refresh when something changes. we do not create another announcement
	because it is not needed given that the only thing that can happen is
	for a new announcement to be recorded"
	self announcer announce: aSignal
]

{ #category : 'initialization' }
CircularMemoryLogger >> reset [
	"it simply removes all recordings without
	affecting the running status of the logger"
	mutex critical: [ self basicReset ]
]
