Class {
	#name : 'ClapDbgStepRelatedCommand',
	#superclass : 'ClapDbgCommand',
	#category : 'Debugger-CLI-Commands',
	#package : 'Debugger-CLI',
	#tag : 'Commands'
}

{ #category : 'execution' }
ClapDbgStepRelatedCommand >> basicExecuteOn: aDebugger with: args [

	[ self performStepOn: aDebugger ]
		on: ProcessAlreadyTerminating
		do: [ :ex | aDebugger finishedDebugging ].

	aDebugger context: aDebugger process suspendedContext.

	aDebugger context ifNil: [ ^ aDebugger finishedDebugging ].

	^ ClapDbgPrintCodeCommand new basicExecuteOn: aDebugger
]

{ #category : 'execution' }
ClapDbgStepRelatedCommand >> performStepOn: aDebugger [
    self subclassResponsibility
]
