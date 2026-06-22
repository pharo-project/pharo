Class {
	#name : 'ClapDbgPrintCommand',
	#superclass : 'ClapDbgCommand',
	#category : 'Debugger-CLI',
	#package : 'Debugger-CLI'
}

{ #category : 'printing' }
ClapDbgPrintCommand >> executeOn: aDebugger with: args [

	| evaluation |
	args trim ifEmpty: [ ^ 'Nothing to print' ].
	[
		evaluation := OpalEvaluator new
			              context: aDebugger context;
			              evaluate: args.
		^ args , ' : ' , evaluation asString ]
		on: Error
		do: [ :ex | ^ ex stackTraceString ]
]

{ #category : 'initialization' }
ClapDbgPrintCommand >> initialize [

	super initialize.
	self commandName: #( 'print' 'p' ).
	self commandHelp: 'evaluate the expression'.
	self arguments: '<expression>'
]
