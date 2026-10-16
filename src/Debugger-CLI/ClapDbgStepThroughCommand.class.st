Class {
	#name : 'ClapDbgStepThroughCommand',
	#superclass : 'ClapDbgStepRelatedCommand',
	#category : 'Debugger-CLI',
	#package : 'Debugger-CLI'
}

{ #category : 'initialization' }
ClapDbgStepThroughCommand >> initialize [

	super initialize.
	self commandName: #( 'stepThrough' 'st' ).
	self commandHelp: 'execute next stepThrough'.
	self arguments: ''
]

{ #category : 'execution' }
ClapDbgStepThroughCommand >> performStepOn: aDebugger [

	aDebugger session stepThrough
]
