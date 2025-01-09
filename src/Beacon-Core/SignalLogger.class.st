"
It represents the abstract class of a `SignalLogger` that takes the signals going through the `Beacon` and actually does something useful.

The subclasses provide concrete implementations. Each of these sublasses can be used both by directly instantiating the class, or by using the `instance` class variable which holds a global instance of that class.
The global instance of a signal logger is useful to provide a simple means to start and stop without worrying about the instance.

For example, consider:

```
   TranscriptLogger start.
   StringSignal emit: 'test'.
   ...
   TranscriptLogger stop.
```	
Passing stop, unsubscribes the global instance of `TranscriptLogger` from the central `Beacon` instance without having to remember the `TranscriptLogger` object.

One can get signals go to a given logger during the execution of a block with:

```
   (MemoryLogger named: 'loggerA')
       runDuring: [ 
         'Hello' asBeaconSignal emit.
         42 asBeaconSignal emit.
         ContextStackSignal emit.
         ];
       recordings.
```		
This can be done for a specific set of signals with:

```
   (MemoryLogger named: 'loggerA')
      runFor: StringSignal, WrapperSignal 
      during: [ 
         'Hello' asBeaconSignal emit.
         42 asBeaconSignal emit.
         StringSignal emit: 'let me pass'
         ];
      recordings.
```
		
It is possible to have signals go to several loggers by nesting `#runDuring:` blocks.
Now this is not replacing a composite logger.

"
Class {
	#name : 'SignalLogger',
	#superclass : 'Object',
	#instVars : [
		'beacon',
		'name'
	],
	#classInstVars : [
		'instance'
	],
	#category : 'Beacon-Core-Loggers',
	#package : 'Beacon-Core',
	#tag : 'Loggers'
}

{ #category : 'instance creation' }
SignalLogger class >> for: aBeacon [

	^ self new
		beacon: aBeacon;
		yourself
]

{ #category : 'accessing' }
SignalLogger class >> instance [

	^ instance ifNil: [ instance := self new ]
]

{ #category : 'testing' }
SignalLogger class >> isAbstract [

	^ self == SignalLogger
]

{ #category : 'accessing' }
SignalLogger class >> named: aString [

	^ (Beacon loggersOf: self)
		  detect: [ :each | each name = aString ]
		  ifNone: [ self new name: aString ]
]

{ #category : 'class initialization' }
SignalLogger class >> resetAllInstances [
	<script>

	self withAllSubclassesDo: [ :each | each resetInstance ]
]

{ #category : 'class initialization' }
SignalLogger class >> resetInstance [

	<script>

	instance ifNotNil: [
		Beacon instance unsubscribe: instance.
		instance := nil ]
]

{ #category : 'registration' }
SignalLogger class >> runDuring: aBlock [

	^ self instance runDuring: aBlock
]

{ #category : 'registration' }
SignalLogger class >> start [
	<script>

	^ self instance start
]

{ #category : 'registration' }
SignalLogger class >> startFor: anAnnouncementOrASetOfAnnouncements [
	"Start listening to announcements.

		logger startFor: StringSignal

	to scope the listening consider using

		logger runFor: StringSignal during: [ ... ]"

	^ self instance startFor: anAnnouncementOrASetOfAnnouncements
]

{ #category : 'convenience' }
SignalLogger class >> startSingleGlobalLogger [
	"Stop any other loggers and start a global instances of the receiver"
	<script>

	SignalLogger
		stopAll;
		resetAllInstances.
	self start
]

{ #category : 'registration' }
SignalLogger class >> stop [
	<script>

	^ self instance stop
]

{ #category : 'registration' }
SignalLogger class >> stopAll [
	<script>

	self withAllSubclassesDo: #stop
]

{ #category : 'accessing' }
SignalLogger >> beacon [

	^ beacon
]

{ #category : 'accessing' }
SignalLogger >> beacon: anAnnouncer [

	beacon := anAnnouncer
]

{ #category : 'initialization' }
SignalLogger >> initialize [

	super initialize.
	"by default we hook to the central ==Beacon== instance"
	beacon := Beacon instance.
	name := 'unnamed'
]

{ #category : 'testing' }
SignalLogger >> isRunning [
	Beacon instance announcer subscriptions
		subscriptionsOf: self do: [ :x | ^ true ].
	^ false
]

{ #category : 'accessing' }
SignalLogger >> name [

	^ name
]

{ #category : 'accessing' }
SignalLogger >> name: aString [

	name := aString
]

{ #category : 'utilities' }
SignalLogger >> nextPut: aSignal [
	"Gets a BeaconSignal into the logger"

	self subclassResponsibility
]

{ #category : 'utilities' }
SignalLogger >> nextPutAll: aCollection [

	aCollection do: [ :each | self nextPut: each ]
]

{ #category : 'printing' }
SignalLogger >> printOn: aStream [

	super printOn: aStream.
	aStream
		nextPutAll: ' named: ';
		nextPutAll: name asString
]

{ #category : 'registration' }
SignalLogger >> runDuring: aBlock [
	"Make all BeaconSignals go to me during aBlock"

	self start.
	[ aBlock value ]
		ensure: [ self stop ]
]

{ #category : 'registration' }
SignalLogger >> runFor: anAnnouncementOrASetOfAnnouncements during: aBlock [
	"Makes all provided BeaconSignals got to me during aBlock.

	logger runFor: StringSignal during: [ ... ]"

	self startFor: anAnnouncementOrASetOfAnnouncements.
	[ aBlock value ] ensure: [ self stop ]
]

{ #category : 'registration' }
SignalLogger >> start [

	self startFor: BeaconSignal
]

{ #category : 'registration' }
SignalLogger >> startFor: anAnnouncementOrASetOfAnnouncements [
	"Make sure that when anAnnouncementOrASetOfAnnouncements is signaled, I receive them as argument of nextPut:."

	self isRunning ifTrue: [ self stop ].
	self beacon
		when: anAnnouncementOrASetOfAnnouncements
		send: #nextPut:
		to: self
]

{ #category : 'registration' }
SignalLogger >> stop [

	self beacon unsubscribe: self
]
