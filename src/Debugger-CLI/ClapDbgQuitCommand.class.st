Class {
	#name : 'ClapDbgQuitCommand',
	#superclass : 'ClapDbgCommand',
	#category : 'Debugger-CLI',
	#package : 'Debugger-CLI'
}

{ #category : 'initialization' }
ClapDbgQuitCommand >> executeOn: aDebugger with: args [

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
