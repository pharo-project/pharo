Class {
	#name : 'ClapDbgProceedCommand',
	#superclass : 'ClapDbgStepRelatedCommand',
	#category : 'Debugger-CLI',
	#package : 'Debugger-CLI'
}

{ #category : 'initialization' }
ClapDbgProceedCommand >> initialize [

	super initialize.
	self commandName: #( 'proceed' 'pr' ).
	self commandHelp: 'Proceed execution until next breakpoint'.
	self arguments: ''
]

{ #category : 'execution' }
ClapDbgProceedCommand >> performStepOn: aDebugger [
	"We proceed execution by stepping over the bottom context.
	This allows us to not loose control on the process.
	
	By construction the bottom context is single message to a single block value.
	
	Do not use #resume, which asynchronously launches the process.
	In that case the process will execute concurrently to the debugger and we will not be able to manage errors."

	aDebugger session completeTo: aDebugger context bottomContext
]
