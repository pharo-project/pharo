"
I am a notification for logging of textual represenation of Metacello events
"
Class {
	#name : 'MetacelloNotification',
	#superclass : 'SystemNotification',
	#instVars : [
		'duration'
	],
	#category : 'Metacello-Core-Exceptions',
	#package : 'Metacello-Core',
	#tag : 'Exceptions'
}

{ #category : 'exceptioninstantiator' }
MetacelloNotification class >> logTimeDuring: aBlock description: description [

	| startTime endTime |
	startTime := Time millisecondClockValue.

	^ aBlock ensure: [ 
		endTime := Time millisecondClockValue.
		self new
			messageText: description;
			milliSeconds: endTime - startTime;
			signal ].
]

{ #category : 'exceptioninstantiator' }
MetacelloNotification class >> signal: message level: aLevel [
	"Signal the occurrence of an exceptional condition with a specified textual description."
	
	| outdent |
	
	outdent := ((1 to: (aLevel - 1)) collect: [ :ignore | String tab ]) joinUsing: ''.

	^ super signal: outdent, message
]

{ #category : 'exceptioninstantiator' }
MetacelloNotification class >> signal: description milliSeconds: milliSeconds [

	^ self new
		  messageText: description;
		  milliSeconds: milliSeconds;
		  signal
]

{ #category : 'accessing' }
MetacelloNotification >> messageText [ 
	
	^ duration 
		ifNil: [ super messageText ]
		ifNotNil: [ '{1} ({2})' format: { super messageText. duration asDuration humanReadablePrintString } ]
]

{ #category : 'instance creation simple' }
MetacelloNotification >> milliSeconds: aValue [

	duration := aValue milliSeconds 
]
