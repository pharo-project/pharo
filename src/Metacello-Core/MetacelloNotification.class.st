"
I am a notification for logging of textual represenation of Metacello events
"
Class {
	#name : 'MetacelloNotification',
	#superclass : 'SystemNotification',
	#category : 'Metacello-Core-Exceptions',
	#package : 'Metacello-Core',
	#tag : 'Exceptions'
}

{ #category : 'exceptioninstantiator' }
MetacelloNotification class >> signal: message level: aLevel [
	"Signal the occurrence of an exceptional condition with a specified textual description."
	
	| outdent |
	
	outdent := ((1 to: (aLevel - 1)) collect: [ :ignore | String tab ]) joinUsing: ''.

	^ super signal: outdent, message
]
