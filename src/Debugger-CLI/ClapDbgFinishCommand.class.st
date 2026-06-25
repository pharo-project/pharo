Class {
	#name : 'ClapDbgFinishCommand',
	#superclass : 'ClapDbgStepRelatedCommand',
	#category : 'Debugger-CLI',
	#package : 'Debugger-CLI'
}

{ #category : 'execution' }
ClapDbgFinishCommand >> initialize [

	super initialize.
	self commandName: #( 'finish' 'f' ).
	self commandHelp: 'executes the whole function'.
	self arguments: ''
]

{ #category : 'execution' }
ClapDbgFinishCommand >> performStepOn: aDebugger [
    
	"Finishing the current method is equivalent to stepping over its caller, which is suspended calling the current method"
	aDebugger session stepOver: aDebugger context sender
]
