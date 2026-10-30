Class {
	#name : 'ClapDbgStepCommand',
	#superclass : 'ClapDbgStepRelatedCommand',
	#category : 'Debugger-CLI-Commands',
	#package : 'Debugger-CLI',
	#tag : 'Commands'
}

{ #category : 'initialization' }
ClapDbgStepCommand >> initialize [

	super initialize.
	self commandName: #( 'step' 's' ).
	self commandHelp: 'execute next stepInto'.
	self arguments: ''
]

{ #category : 'execution' }
ClapDbgStepCommand >> performStepOn: aDebugger [

	aDebugger session stepInto
]
