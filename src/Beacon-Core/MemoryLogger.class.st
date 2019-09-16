"
This signal logger simply records the signals that it receives.

!!Example1: Instance usage
[[[
	(MemoryLogger new 
		runDuring: [ 
			StringSignal emit: 'This is a message' ]	)
				inspect.
]]]

!!Example 2: Global usage
[[[
	MemoryLogger reset.
	MemoryLogger start.
	StringSignal emit: 'This is a message' .
	MemoryLogger instance recordings inspect.
	MemoryLogger stop.
]]]
"
Class {
	#name : #MemoryLogger,
	#superclass : #SignalLogger,
	#instVars : [
		'recordings',
		'announcer',
		'mutex'
	],
	#classVars : [
		'WriteFilename',
		'WritePeriod',
		'WriteProcess'
	],
	#category : #'Beacon-Core-Loggers'
}

{ #category : #registering }
MemoryLogger class >> reset [
	self instance reset
]

{ #category : #saving }
MemoryLogger class >> startAutoSaving [
	"Write the default instance to a file every WritePeriod"

	WriteProcess ifNotNil: [ self error: 'Write process already running' ].

	WriteProcess := [ self writeLoop ]
		forkAt: Processor userInterruptPriority
		named: self class name, ' auto-save'.
]

{ #category : #saving }
MemoryLogger class >> stopAutoSaving [
	"Terminate writing the default instance to a file every WritePeriod"

	WriteProcess ifNil: [ ^self ].
	WriteProcess terminate.
	100 milliSeconds wait.
	WriteProcess isTerminated ifFalse: [ self error: 'Unable to terminate auto-save process' ].
	WriteProcess := nil.
]

{ #category : #accessing }
MemoryLogger class >> writeFilename [

	^WriteFilename ifNil: [ WriteFilename := String streamContents: [ :stream |
			stream 
				<< self name;
				<< '-';
				print: LibC uniqueInstance currentProcessId;
				<< '.stgz' ] ].
]

{ #category : #accessing }
MemoryLogger class >> writeFilename: aFilename [

	WriteFilename := aFilename 
]

{ #category : #private }
MemoryLogger class >> writeLoop [
	"Write the default instance out to a file every WritePeriod"

	[ self instance saveStonFile: self writeFilename.
	self writePeriod wait ] repeat.
]

{ #category : #accessing }
MemoryLogger class >> writePeriod [

	^WritePeriod ifNil: [ WritePeriod := 60 seconds ]
]

{ #category : #accessing }
MemoryLogger class >> writePeriod: aDuration [

	WritePeriod := aDuration
]

{ #category : #accessing }
MemoryLogger >> announcer [ 
	"The announcer announces to potential watchers (typically viewers)
	that a new signal was recorded"
	^ announcer
]

{ #category : #initialize }
MemoryLogger >> initialize [
	super initialize.
	mutex := Mutex new.
	self reset.
	announcer := Announcer new.

]

{ #category : #accessing }
MemoryLogger >> nextPut: aSignal [
	mutex critical: [
		recordings add: aSignal ].
	"the announcement is passed forward so that potential viewers
	can refresh when something changes. we do not create another announcement
	because it is not needed given that the only thing that can happen is
	for a new announcement to be recorded"
	self announcer announce: aSignal
]

{ #category : #accessing }
MemoryLogger >> recordings [
	"It answers a copy of the recorded collection to not grant
	access to the users to a shared resource that should only be 
	modified via the mutex"
	^ recordings copy
]

{ #category : #initialize }
MemoryLogger >> reset [
	"it simply removes all recordings without
	affecting the running status of the logger"
	mutex critical: [
		recordings := OrderedCollection new ]
]
