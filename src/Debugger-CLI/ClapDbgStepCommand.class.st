Class {
	#name : 'ClapDbgStepCommand',
	#superclass : 'ClapDbgStepRelatedCommand',
	#category : 'Debugger-CLI',
	#package : 'Debugger-CLI'
}

{ #category : 'initialization' }
ClapDbgStepCommand >> initialize [

	super initialize.
	self commandName: #( 'step' 's' ).
	self commandHelp: 'execute next stepInto'.
	self arguments: ''
]

{ #category : 'command execution' }
ClapDbgStepCommand >> performStepOn: aDebugger [
    aDebugger session stepInto
]
