"
This is the central and the dummiest class of the Beacon logging system.

It's a singleton class that holds an announcer instance. Its goal is purely to serve as a central beacon through which all signals pass. It works together with `Announcement>>#emit`.

By itself, it does not do anything useful. Other `SignalLogger`s are meant to register to its announcer and link the announcements to something useful.
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
