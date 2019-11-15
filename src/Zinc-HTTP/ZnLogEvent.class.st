"
I am ZnLogEvent, the base class of a log events emitted by elements of the Zinc HTTP Components framework.

I add a timestamp and a simple id attribute. The id can wrap around and should only be used to distinguish between events that have the same timestamp.

ZnLogEvents are distributed as Announcement through a singleton Announcer that I maintain.

I have a small convenience API to log to the Transcript or open a simple GUI on the emitted log events.
"
Class {
	#name : #ZnLogEvent,
	#superclass : #Announcement,
	#instVars : [
		'timestamp',
		'id'
	],
	#classVars : [
		'IdCounter',
		'LogEventAnnouncer'
	],
	#category : #'Zinc-HTTP-Logging'
}

{ #category : #accessing }
ZnLogEvent class >> announcer [
	^ LogEventAnnouncer ifNil: [ LogEventAnnouncer := Announcer new ]
]

{ #category : #'class initialization' }
ZnLogEvent class >> initialize [
	IdCounter := 0.
	self environment 
		at: #SessionManager 
		ifPresent: [ :manager | manager default registerNetworkClassNamed: self name ]
		ifAbsent: [ Smalltalk addToStartUpList: self ]
]

{ #category : #convenience }
ZnLogEvent class >> logToTranscript [
	self stopLoggingToTranscript.
	^ self announcer when: ZnLogEvent do: [ :event | self crTrace: event ].
]

{ #category : #accessing }
ZnLogEvent class >> nextId [
	"This should be thread safe because SmallInteger 
	arithmetic primitives cannot be interrupted"
	
	^ IdCounter := IdCounter + 1
]

{ #category : #convenience }
ZnLogEvent class >> open [
	^ self announcer inspect
]

{ #category : #'system startup' }
ZnLogEvent class >> startUp [
	"Reset the id counter"
	
	IdCounter := 0
]

{ #category : #convenience }
ZnLogEvent class >> stopLoggingToTranscript [
	self announcer unsubscribe: self
]

{ #category : #accessing }
ZnLogEvent >> announcer [
	^ self class announcer
]

{ #category : #actions }
ZnLogEvent >> emit [
	self announcer announce: self
]

{ #category : #accessing }
ZnLogEvent >> id [
	^ id
]

{ #category : #initialize }
ZnLogEvent >> initialize [
	super initialize.
	timestamp := DateAndTime now.
	id := self nextId
]

{ #category : #accessing }
ZnLogEvent >> nextId [
	^ self class nextId
]

{ #category : #printing }
ZnLogEvent >> printContentsOn: stream [
	"Subclasses should implement this to add output"
]

{ #category : #printing }
ZnLogEvent >> printOn: stream [
	timestamp printYMDOn: stream.
	stream space.
	timestamp printHMSOn: stream. 
	stream space.
	id \\ 1000 printOn: stream base: 10 length: 3 padded: true.
	self printContentsOn: stream
]

{ #category : #accessing }
ZnLogEvent >> timestamp [
	^ timestamp
]
