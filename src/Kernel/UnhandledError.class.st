"
I am the ultimate error. 
By default if an Error or Exception is not handled by the code the default action is to raise an UnhandledError which in interactive mode triggers the UIManager to open a debugger.

	Error signal
	...
	UnhandledError signalForException: error
	...
	UIManager opens a debugger
"
Class {
	#name : #UnhandledError,
	#superclass : #Exception,
	#instVars : [
		'exception'
	],
	#category : #Kernel-Exceptions
}

{ #category : #signalling }
UnhandledError class >> signalForException: anError [

	^ self new
		exception: anError;
		signal
]

{ #category : #'priv handling' }
UnhandledError >> defaultAction [
	<reflective: #unhandledErrorDefaultAction:message:>
 	^ UIManager default unhandledErrorDefaultAction: self exception
]

{ #category : #accessing }
UnhandledError >> exception [

	^ exception
]

{ #category : #accessing }
UnhandledError >> exception: anError [

	exception := anError
]

{ #category : #'priv handling' }
UnhandledError >> isResumable [
	
	^ false
]
