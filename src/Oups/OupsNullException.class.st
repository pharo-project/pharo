"
All debuggers are opened from an exception.
I am an exception built in case the opening of a debugger is requested without an exception (user interrupt, debugIt..).
"
Class {
	#name : #OupsNullException,
	#superclass : #Exception,
	#category : #'Oups-exceptions'
}

{ #category : #'instance creation' }
OupsNullException class >> fromSignallerContext: aContext [
	^self new signalIn: aContext
]

{ #category : #accessing }
OupsNullException >> description [
	^'I am an Oups NULL debugging exception'
]

{ #category : #signaling }
OupsNullException >> signalIn: context [
	"This exception is not signalled but started from a given context, that is its direct signaller context. Therefore the exception is not signalled after setting its signal context"
	signalContext := context.
	signaler ifNil: [ signaler := self receiver ]
]
