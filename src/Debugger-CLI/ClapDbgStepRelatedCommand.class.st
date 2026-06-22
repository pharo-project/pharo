Class {
	#name : 'ClapDbgStepRelatedCommand',
	#superclass : 'ClapDbgCommand',
	#category : 'Debugger-CLI',
	#package : 'Debugger-CLI'
}

{ #category : 'execution' }
ClapDbgStepRelatedCommand >> executeOn: aDebugger with: args [

	[ self performStepOn: aDebugger ]
		on: ProcessAlreadyTerminating
		do: [ :ex | aDebugger finishedDebugging ].

	aDebugger context: aDebugger process suspendedContext.

	aDebugger context ifNil: [ ^ aDebugger finishedDebugging ].

	^ ClapDbgPrintCodeCommand new executeOn: aDebugger with: ''
]

{ #category : 'execution' }
ClapDbgStepRelatedCommand >> performStepOn: aDebugger [
    self subclassResponsibility
]
