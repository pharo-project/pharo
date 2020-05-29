"
I am a root of hierarchy of various kinds of ultimate exceptions. 
The ultimate meaning here is that such exceptions execute the UI action to interrupt the process execution as the result of not being handled. So no other exceptions are be raisen there. 

In interactive mode whey will trigger the UIManager to open a debugger.

There are two main subclasses:
- Halt to break and debug execution.
- UnhandledError to give a last chance to handle given exception 
See comments for details
"
Class {
	#name : #UnhandledException,
	#superclass : #Exception,
	#category : #'Kernel-Exceptions'
}

{ #category : #handling }
UnhandledException >> defaultAction [
	"If none handles this either then open debugger.
	
	Note about naming here: 
	Even if not all exceptions are errors the absence of a handler for signaled exception is considered as an error by default. Therefore #defaultAction for UnhandledException performs the #unhandledErrorAction"
 	^ self unhandledErrorAction
]
