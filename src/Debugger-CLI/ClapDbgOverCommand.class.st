Class {
	#name : 'ClapDbgOverCommand',
	#superclass : 'ClapDbgStepRelatedCommand',
	#category : 'Debugger-CLI-Commands',
	#package : 'Debugger-CLI',
	#tag : 'Commands'
}

{ #category : 'initialization' }
ClapDbgOverCommand >> initialize [

	super initialize.
	self commandName: #( 'over' 'o' ).
	self commandHelp: 'execute next stepOver'.
	self arguments: ''
]

{ #category : 'execution' }
ClapDbgOverCommand >> performStepOn: aDebugger [

	aDebugger session stepOver
]
