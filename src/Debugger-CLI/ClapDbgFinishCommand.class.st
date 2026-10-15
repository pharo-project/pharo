Class {
	#name : 'ClapDbgFinishCommand',
	#superclass : 'ClapDbgStepRelatedCommand',
	#category : 'Debugger-CLI-Commands',
	#package : 'Debugger-CLI',
	#tag : 'Commands'
}

{ #category : 'initialization' }
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
