"
A GLMPortEvent is passed to the parent pane every time a Port changes.

Instance Variables
	oldValue:		<Object>
	port:		Port
	transmission:		Transmission
	transmissionContext:	TransmissionContext
"
Class {
	#name : #GLMPortEvent,
	#superclass : #GLMLoggedObject,
	#instVars : [
		'port',
		'oldValue',
		'transmissionContext'
	],
	#category : #'Glamour-Core'
}

{ #category : #'instance creation' }
GLMPortEvent class >> on: aPort previouslyValued: anObject [ 
	^ self new 
		initializeOn: aPort
		previouslyValued: anObject
]

{ #category : #'instance creation' }
GLMPortEvent class >> on: aPort previouslyValued: anObject in: aTransmissionContext [ 
	^self new initializeOn: aPort previouslyValued: anObject in: aTransmissionContext 
]

{ #category : #testing }
GLMPortEvent >> hasChanged [
	"Answer whether the port value changed due to the event."

	^ self value ~~ self oldValue
]

{ #category : #initialization }
GLMPortEvent >> initializeOn: aPort previouslyValued: anObject [ 
	self initializeOn: aPort previouslyValued: anObject in: OrderedCollection new
]

{ #category : #'initialize-release' }
GLMPortEvent >> initializeOn: aPort previouslyValued: anObject in: aTransmissionContext [ 
	port := aPort.
	oldValue := anObject.
	transmissionContext := aTransmissionContext
]

{ #category : #testing }
GLMPortEvent >> isEntityPort [
	^ self port isEntityPort
]

{ #category : #testing }
GLMPortEvent >> isSelectionPort [
	^ self port isSelectionPort
]

{ #category : #accessing }
GLMPortEvent >> oldValue [ 
	 
	^oldValue
]

{ #category : #'accessing-convenience' }
GLMPortEvent >> pane [ 
	 
	^self port pane
]

{ #category : #accessing }
GLMPortEvent >> port [ 
	 
	^port
]

{ #category : #'accessing-convenience' }
GLMPortEvent >> portName [ 
	 
	^self port name
]

{ #category : #printing }
GLMPortEvent >> printOn: aStream [
	super printOn: aStream.
	aStream 
		nextPutAll: '(port='.
	self port printOn: aStream.
	aStream nextPutAll: ' oldValue='; nextPutAll: self oldValue printString; nextPut: $)
		
]

{ #category : #accessing }
GLMPortEvent >> transmissionContext [
	^ transmissionContext
]

{ #category : #'accessing-convenience' }
GLMPortEvent >> value [ 
	 
	^self port value
]
