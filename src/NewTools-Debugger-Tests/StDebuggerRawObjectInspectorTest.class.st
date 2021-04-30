Class {
	#name : #StDebuggerRawObjectInspectorTest,
	#superclass : #TestCase,
	#category : #'NewTools-Debugger-Tests-Presenters'
}

{ #category : #tests }
StDebuggerRawObjectInspectorTest >> testSelectedObject [

	| context debuggerContext inspector |
	context := Context
		           sender: thisContext
		           receiver: Object new
		           method: Object >> #yourself
		           arguments: #(  ).
	debuggerContext := StDebuggerContext context: context.
	inspector := StDebuggerRawObjectInspector onObject: debuggerContext.
	self assert: inspector selectedObject identicalTo: context receiver
]
