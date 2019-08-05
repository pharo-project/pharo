Class {
	#name : #MyHalt,
	#superclass : #Exception,
	#category : #'Debugger-Tests'
}

{ #category : #accessing }
MyHalt >> defaultAction [
	"The default action taken if the exception is signaled."
	"Mimics the normal way an exception being signaled results in its process being suspended and a DebugSession being created."

	| debugSession |
	debugSession := Processor activeProcess newDebugSessionNamed: 'test' startedAt: self signalerContext.
	"Storing the debug session in this class variable so that it can be recovered by the test"
	DebugSessionContexts2Test debugSession: debugSession.
	Processor activeProcess suspend.
]
