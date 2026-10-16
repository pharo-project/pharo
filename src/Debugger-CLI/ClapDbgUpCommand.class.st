Class {
	#name : 'ClapDbgUpCommand',
	#superclass : 'ClapDbgCommand',
	#category : 'Debugger-CLI',
	#package : 'Debugger-CLI'
}

{ #category : 'execution' }
ClapDbgUpCommand >> basicExecuteOn: aDebugger with: args [

	| currentContext callee |
	currentContext := aDebugger context.
	callee := aDebugger process suspendedContext findContextSuchThat: [ :context | context hasSender: currentContext ].

	callee ifNil: [ ^ 'Already at the top of the stack' ] ifNotNil: [ :c | aDebugger context: c ].

	^ ClapDbgPrintCodeCommand new executeOn: aDebugger with: ''
]

{ #category : 'initialization' }
ClapDbgUpCommand >> initialize [

	super initialize.
	self commandName: #( 'up' 'u' ).
	self commandHelp: 'go up in the stack'.
	self arguments: ''
]
