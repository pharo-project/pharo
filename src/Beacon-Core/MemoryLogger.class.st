"
This signal logger simply records the signals that it receives.

###Example1: Instance usage

```
	(MemoryLogger new 
		runDuring: [ 
			StringSignal emit: 'This is a message' ]	)
				inspect.
```

###Example 2: Global usage

```
	MemoryLogger reset.
	MemoryLogger start.
	StringSignal emit: 'This is a message' .
	MemoryLogger instance recordings inspect.
	MemoryLogger stop.
```
"
Class {
	#name : 'MemoryLogger',
	#superclass : 'SignalLogger',
	#instVars : [
		'announcer',
		'mutex',
		'entries'
	],
	#classVars : [
		'WriteFilename',
		'WritePeriod',
		'WriteProcess'
	],
	#category : 'Beacon-Core-Loggers',
	#package : 'Beacon-Core',
	#tag : 'Loggers'
}

{ #category : 'saving' }
MemoryLogger class >> fromStonFile: aFileReference [

	| logger |

	logger := aFileReference asFileReference binaryReadStreamDo: [ :bstream |
		STON fromStream: (ZnCharacterReadStream
			on: (GZipReadStream on: bstream)
			encoding: 'utf8') ].
	^logger
]

{ #category : 'class initialization' }
MemoryLogger class >> reset [
	<script>

	self instance reset
]

{ #category : 'saving' }
MemoryLogger class >> startAutoSaving [
	"Write the default instance to a file every WritePeriod"
	<script>

	WriteProcess ifNotNil: [ self error: 'Write process already running' ].

	WriteProcess := [ self writeLoop ]
		                forkAt: Processor userInterruptPriority
		                named: self class name , ' auto-save'
]

{ #category : 'saving' }
MemoryLogger class >> stopAutoSaving [
	"Terminate writing the default instance to a file every WritePeriod"
	<script>

	WriteProcess ifNil: [ ^ self ].
	WriteProcess terminate.
	100 milliSeconds wait.
	WriteProcess isTerminated ifFalse: [
		self error: 'Unable to terminate auto-save process' ].
	WriteProcess := nil
]

{ #category : 'accessing' }
MemoryLogger class >> writeFilename [

	^ WriteFilename ifNil: [
		  WriteFilename := String streamContents: [ :stream |
			                   stream
				                   << self name;
				                   << '-';
				                   print: LibC uniqueInstance currentProcessId;
				                   << '.stgz' ] ]
]

{ #category : 'accessing' }
MemoryLogger class >> writeFilename: aFilename [

	WriteFilename := aFilename
]

{ #category : 'private' }
MemoryLogger class >> writeLoop [
	"Write the default instance out to a file every WritePeriod"

	[ self instance saveStonFile: self writeFilename.
	self writePeriod wait ] repeat
]

{ #category : 'accessing' }
MemoryLogger class >> writePeriod [

	^ WritePeriod ifNil: [ WritePeriod := 60 seconds ]
]

{ #category : 'accessing' }
MemoryLogger class >> writePeriod: aDuration [

	WritePeriod := aDuration
]

{ #category : 'accessing' }
MemoryLogger >> announcer [
	"The announcer announces to potential watchers (typically viewers)
	that a new signal was recorded"
	^ announcer
]

{ #category : 'accessing' }
MemoryLogger >> entries [
	"It answers a copy of the recorded collection to not grant
	access to the users to a shared resource that should only be
	modified via the mutex"

	^ entries copy
]

{ #category : 'accessing' }
MemoryLogger >> entryCount [

	^ entries size
]

{ #category : 'testing' }
MemoryLogger >> hasEntries [

	^ entries isNotEmpty 
]

{ #category : 'initialization' }
MemoryLogger >> initialize [
	super initialize.
	mutex := Mutex new.
	self reset.
	announcer := Announcer new
]

{ #category : 'inspector' }
MemoryLogger >> inspectionEntries: aBuilder [
	<inspectorPresentationOrder: 10 title: 'Entries'>

	^ aBuilder newList 
		addStyle: 'stList';
		items: entries;
		yourself
]

{ #category : 'logging' }
MemoryLogger >> nextPut: aSignal [

	mutex critical: [
		entries add: aSignal ].
	"the announcement is passed forward so that potential viewers
	can refresh when something changes. we do not create another announcement
	because it is not needed given that the only thing that can happen is
	for a new announcement to be recorded"
	self announcer announce: aSignal
]

{ #category : 'accessing' }
MemoryLogger >> recordings [

	self
		deprecated: 'Use entries'
		on: '28/03/2025'
		in: 'pharo13'
		transformWith: '`@receiver recordings'
						-> '`@receiver entries'.
	^ self entries 
]

{ #category : 'initialization' }
MemoryLogger >> reset [
	"it simply removes all entries without
	affecting the running status of the logger"
	mutex critical: [
		entries := OrderedCollection new ]
]

{ #category : 'ston persistence' }
MemoryLogger >> saveStonFile: aFileReference [

	"Save the receiver to the supplied file name"

	| stream |
	stream := ZnCharacterWriteStream
		          on: (GZipWriteStream on: aFileReference asFileReference nextVersion binaryWriteStream)
		          encoding: 'utf8'.
	[ STON put: self onStreamPretty: stream ] ensure: [ stream close ]
]
