"
I am a raw SDL2 event. 

To understand my purpose, it's important to know that, in SDL2, multiple related ""event types"" often reuse the same struct to represent their event. For example, both the SDL_KEYDOWN and SDL_KEYUP types use the SDL_KeyboardEvent structure. Fundamentally, I map such an event type to its appropriate structure.

From the Pharo perspective, my SDL2 implementation might seem a bit odd. I am an external C union with one field for each and every type of SDL2 event, so although I'm called an ""event"", I'm really a collection containing an instance of every event type. Since only one of my elements is relevant for any particular event (the rest can be ignored), I also provide the event ""type"" code (a C uint) to specify that element. In SDL2, the user would usually then have to take that code and manually map it to my correct element (the documentation suggests a giant switch statement - yay!), but luckily, in Pharo, I can be #mapped, which will return an instance of the correct event struct that will represent my individual ""event"".

For further information, see the documentation at https://wiki.libsdl.org/SDL_Event of which this comment is a synopsis.
"
Class {
	#name : #'SDL_Event',
	#superclass : #FFIExternalObject,
	#classVars : [
		'EventTypeMap'
	],
	#category : #'OSWindow-SDL2-Bindings'
}

{ #category : #examples }
SDL_Event class >> exampleKeyDownEvent [
	<sampleInstance>
	^ SDL_KeyDownEvent example
]

{ #category : #examples }
SDL_Event class >> exampleKeyDownEventInvalidWindowID [
	<sampleInstance>
	^ SDL_KeyDownEvent exampleWithInvalidWindowID
]

{ #category : #examples }
SDL_Event class >> exampleUnknownEvent [
	<sampleInstance>
	^ self new
		type: 0;
		yourself
]

{ #category : #'class initialization' }
SDL_Event class >> initialize [
	"self initialize"

	SDL2Types initialize.  "make sure its initialized"
	self initializeEventTypeMap
]

{ #category : #'class initialization' }
SDL_Event class >> initializeEventTypeMap [
	"self initializeEventTypeMap"
	EventTypeMap := Dictionary new.
	SDL2MappedEvent  allSubclassesDo: [ :cls |
		| eventType |
		eventType := cls eventType.
		eventType ifNotNil: [ EventTypeMap at: eventType put: cls ] ]

]

{ #category : #initialization }
SDL_Event >> initialize [
	super initialize.
	self setHandle: (ByteArray new: 56)
]

{ #category : #accessing }
SDL_Event >> mapped [
	| eventClass |
	eventClass := EventTypeMap at: self type ifAbsent: [ SDL_CommonEvent ].
	^ eventClass fromSdlEvent: self
]

{ #category : #accessing }
SDL_Event >> type [
	^self getHandle uint32AtOffset: 0
]

{ #category : #private }
SDL_Event >> type: anInteger [
	"Useful for testing e.g. creating dummy events"
	^ self getHandle uint32AtOffset: 0 put: anInteger
]
