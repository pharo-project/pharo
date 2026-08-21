Class {
	#name : 'StepRelatedCommand',
	#superclass : 'DebuggerCommand',
	#category : 'Debugger-CLI',
	#package : 'Debugger-CLI'
}

{ #category : 'execution' }
StepRelatedCommand >> executeOn: aDebugger with: args [

	[ self performStepOn: aDebugger ]
		on: ProcessAlreadyTerminating
		do: [ :ex | aDebugger finishedDebugging ].

	aDebugger context: aDebugger process suspendedContext.

	aDebugger context ifNil: [ ^ aDebugger finishedDebugging ].

	^ self printResultFor: aDebugger
]

{ #category : 'execution' }
StepRelatedCommand >> performStepOn: aDebugger [
    self subclassResponsibility
]

{ #category : 'execution' }
StepRelatedCommand >> printResultFor: aDebugger [
    self subclassResponsibility
]
