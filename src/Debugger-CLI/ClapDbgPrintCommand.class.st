Class {
	#name : 'ClapDbgPrintCommand',
	#superclass : 'ClapDbgCommand',
	#category : 'Debugger-CLI-Commands',
	#package : 'Debugger-CLI',
	#tag : 'Commands'
}

{ #category : 'execution' }
ClapDbgPrintCommand >> basicExecuteOn: aDebugger with: args [

	| evaluation |
	args trim ifEmpty: [ ^ 'Nothing to print' ].
	evaluation := OpalEvaluator new
		              context: aDebugger context;
		              evaluate: args.

	^ evaluation displayString
]

{ #category : 'initialization' }
ClapDbgPrintCommand >> initialize [

	super initialize.
	self commandName: #( 'print' 'p' ).
	self commandHelp: 'evaluate the expression'.
	self arguments: '<expression>'
]
