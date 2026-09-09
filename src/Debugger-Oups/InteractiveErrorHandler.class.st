"
I am an error handler calling the emergency debugger to handle unhandled errors.
"
Class {
	#name : 'InteractiveErrorHandler',
	#superclass : 'ErrorHandler',
	#category : 'Debugger-Oups',
	#package : 'Debugger-Oups'
}

{ #category : 'testing' }
InteractiveErrorHandler class >> isValidForCurrentSystemConfiguration [

	^ Smalltalk isInteractive
]

{ #category : 'error handling' }
InteractiveErrorHandler >> handleError: anError log: shouldLog [

	shouldLog ifTrue: [ self logError: anError ].
	(OupsDebugRequest newForException: anError) submit
]

{ #category : 'error handling' }
InteractiveErrorHandler >> handleNotification: aNotification [

	aNotification messageText traceCr.
	aNotification resume
]

{ #category : 'error handling' }
InteractiveErrorHandler >> handleWarning: aWarning [
	"The user should be notified of the occurrence of an exceptional occurrence and given an option of continuing or aborting the computation. The description of the occurrence should include any text specified as the argument of the #signal: message."

	(OupsWarningRequest newForException: aWarning) submit
]

{ #category : 'error handling' }
InteractiveErrorHandler >> handleWarningDebugRequest: aWarningDebugRequest fromDebuggerSystem: anOupsDebuggerSystem [

	<debuggerCompleteToSender>
	anOupsDebuggerSystem openDebuggerOnRequest: aWarningDebugRequest
]
