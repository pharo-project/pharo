Class {
	#name : 'ClapDbgQuitCommand',
	#superclass : 'ClapDbgCommand',
	#category : 'Debugger-CLI-Commands',
	#package : 'Debugger-CLI',
	#tag : 'Commands'
}

{ #category : 'execution' }
ClapDbgQuitCommand >> basicExecuteOn: aDebugger with: args [

	aDebugger disActivate.
	^ 'Debugging finished'
]

{ #category : 'initialization' }
ClapDbgQuitCommand >> initialize [

	super initialize.
	self commandName: #( 'quit' 'q' ).
	self commandHelp: 'kills the debugger'.
	self arguments: ''
]
