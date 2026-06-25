Class {
	#name : 'ClapDbgPrintCommand',
	#superclass : 'ClapDbgCommand',
	#category : 'Debugger-CLI',
	#package : 'Debugger-CLI'
}

{ #category : 'execution' }
ClapDbgPrintCommand >> basicExecuteOn: aDebugger with: args [

	| evaluation |
	args trim ifEmpty: [ ^ 'Nothing to print' ].
	evaluation := OpalEvaluator new
		              context: aDebugger context;
		              evaluate: args.

	^ evaluation asString
]

{ #category : 'initialization' }
ClapDbgPrintCommand >> initialize [

	super initialize.
	self commandName: #( 'print' 'p' ).
	self commandHelp: 'evaluate the expression'.
	self arguments: '<expression>'
]
