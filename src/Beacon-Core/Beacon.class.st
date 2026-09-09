"
This is the central and the dummiest class of the Beacon logging system.

It's a singleton class that holds an announcer instance. Its goal is purely to serve as a central beacon through which all signals pass.

By itself, it does not do anything useful. `SignalLogger`s are meant to register to its announcer and link the announcements to something useful.

### Beacon architecture

Beacon architectural elements are:
- Domain objects that emit Signals using the message `emit`.
- Signal objects that are announcements that management by the Beacon announcing system.
- The class Beacon that is just an announcement single point managing logger registrations.
- Logger objects that register for local or global sessions of signal reception.


### Simple local logger

The following script creates a logger for the block execution time and get the signals emitted during this period. Here only one. 

```
l := TranscriptLogger new.
l runDuring: [ 'Beacon and Pharo are'  emit ]
```
"
Class {
	#name : 'Beacon',
	#superclass : 'Object',
	#instVars : [
		'announcer'
	],
	#classVars : [
		'Instance'
	],
	#category : 'Beacon-Core-Beacon',
	#package : 'Beacon-Core',
	#tag : 'Beacon'
}

{ #category : 'logging' }
Beacon class >> announce: anAnnouncement [

	self instance announce: anAnnouncement
]

{ #category : 'accessing' }
Beacon class >> instance [

	^ Instance ifNil: [ Instance := self new ]
]

{ #category : 'accessing' }
Beacon class >> loggers [

	^ self instance loggers
]

{ #category : 'accessing' }
Beacon class >> loggersOf: aClass [

	^ self instance loggersOf: aClass
]

{ #category : 'class initialization' }
Beacon class >> reset [
	<script>

	self resetInstance
]

{ #category : 'private - initialization' }
Beacon class >> resetInstance [

	Instance ifNotNil: [
		Instance announcer subscriptions reset ].
	Instance := nil
]

{ #category : 'private' }
Beacon >> announce: anAnnouncement [

	self announcer announce: anAnnouncement
]

{ #category : 'accessing' }
Beacon >> announcer [

	^ announcer
]

{ #category : 'accessing' }
Beacon >> announcer: anObject [

	announcer := anObject
]

{ #category : 'initialization' }
Beacon >> initialize [

	super initialize.
	announcer := Announcer new
]

{ #category : 'accessing' }
Beacon >> loggers [

	^ self loggersOf: SignalLogger
]

{ #category : 'accessing' }
Beacon >> loggersOf: aClass [
	"Returns the loggers of a certain class of loggers."

	^ (announcer subscriptions subscriptions
		   collect: [ :each | each subscriber ]
		   as: Set) select: [ :each | each isKindOf: aClass ]
]

{ #category : 'announcing' }
Beacon >> unsubscribe: aSignalLogger [

	self announcer unsubscribe: aSignalLogger
]

{ #category : 'announcing' }
Beacon >> when: anAnnouncementClass send: aSelector to: anObject [

	self announcer when: anAnnouncementClass send: aSelector to: anObject
]
