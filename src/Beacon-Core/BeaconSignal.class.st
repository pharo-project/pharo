"
A Signal is a special kind of announcement that when emitted is passed to a central Beacon to which SignalLoggers may register. When registered loggers can then handle emitted signals: they can serialize them, store them in memory, or perform other treatments.

By default a signal records the timestamp of its creation and the processId of its creator. 
It is specifically useful in the context of logging, and it serves as the superclass for custom signals. Users are encouraged to capture custom signals in explicit subclasses.

The class also offers a property instance variable that can be used by custom loggers to capture specific logging parameters. Accessors to the concrete entries are meant to be wrapped in setters and getters.

For example, a ClassicLogger might want to add the notion of a level to a signal. To this end, the ClassicLogger package would define:

BeaconSignal >> level
	^ self properties at: #level ifAbsent: [ LogLevel info ]
BeaconSignal >> level: aLevel
	^ self properties at: #level put: aLevel
	
"
Class {
	#name : #BeaconSignal,
	#superclass : #Announcement,
	#instVars : [
		'timestamp',
		'processId',
		'properties'
	],
	#category : #'Beacon-Core-Signals'
}

{ #category : #accessing }
BeaconSignal >> asBeaconSignal [
	^ self
]

{ #category : #initialization }
BeaconSignal >> initialize [
	super initialize.
	timestamp := DateAndTime now.
	processId := Processor activeProcess name.
	properties := Dictionary new
]

{ #category : #accessing }
BeaconSignal >> name [
	^ self class name
]

{ #category : #printing }
BeaconSignal >> printOn: stream [
	self printOneLineOn: stream
]

{ #category : #printing }
BeaconSignal >> printOneLineContentsOn: stream [
	"hook to be overriden by subclasses"
]

{ #category : #printing }
BeaconSignal >> printOneLineOn: stream [
	stream 
		nextPutAll: self timestamp asString;
		space;
		nextPutAll: self class name;
		space.
	self printOneLineContentsOn: stream
]

{ #category : #printing }
BeaconSignal >> printOneLineString [
	^ String streamContents: [:stream | self printOneLineOn: stream]
]

{ #category : #accessing }
BeaconSignal >> processId [
	^ processId
]

{ #category : #accessing }
BeaconSignal >> properties [
	^ properties
]

{ #category : #accessing }
BeaconSignal >> timestamp [
	^ timestamp
]
