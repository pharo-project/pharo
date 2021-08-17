Class {
	#name : #UnhandledExceptionSignalledByADebuggedExecution,
	#superclass : #DebuggedExecutionException,
	#instVars : [
		'unhandledException'
	],
	#category : #'Sindarin-Exceptions'
}

{ #category : #signalling }
UnhandledExceptionSignalledByADebuggedExecution class >> signalWithException: anException [

	self new
		unhandledException: anException;
		signal:
			'An execution debugged by a scriptable debugger signalled an exception that it did not handle itself. You can find this exception is in the #unhandledException instance variable of this object.'
]

{ #category : #accessing }
UnhandledExceptionSignalledByADebuggedExecution >> unhandledException [

	^ unhandledException
]

{ #category : #accessing }
UnhandledExceptionSignalledByADebuggedExecution >> unhandledException: anException [

	unhandledException := anException
]
