Class {
	#name : 'ClapDbgDownCommand',
	#superclass : 'ClapDbgCommand',
	#category : 'Debugger-CLI',
	#package : 'Debugger-CLI'
}

{ #category : 'execution' }
ClapDbgDownCommand >> executeOn: aDebugger with: args [

	aDebugger context sender ifNil: [ ^ 'Already at the bottom of the stack' ] ifNotNil: [ :s | aDebugger context: s ].

	^ ClapDbgPrintCodeCommand new executeOn: aDebugger with: ''
]

{ #category : 'initialization' }
ClapDbgDownCommand >> initialize [

	super initialize.
	self commandName: #( 'down' 'd' ).
	self commandHelp: 'go down in the stack'.
	self arguments: ''
]
