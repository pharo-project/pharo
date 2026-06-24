Class {
	#name : 'ClapDbgReturnCommand',
	#superclass : 'ClapDbgCommand',
	#category : 'Debugger-CLI',
	#package : 'Debugger-CLI'
}

{ #category : 'execution' }
ClapDbgReturnCommand >> executeOn: aDebugger with: args [

	| valueToReturn |
	[
		valueToReturn := OpalEvaluator new
			                 context: aDebugger context;
			                 evaluate: args.
		aDebugger session returnValue: valueToReturn from: aDebugger context.
		aDebugger context: aDebugger process suspendedContext.

		^ ClapDbgPrintCodeCommand new executeOn: aDebugger with: '' ]
		on: Error
		do: [ :ex | ^ self handleError: ex onDebugger: aDebugger ]
]

{ #category : 'initialization' }
ClapDbgReturnCommand >> initialize [

	super initialize.
	self commandName: #( 'return' 'r' ).
	self commandHelp: 'return the expression and exit the function'.
	self arguments: '<expression>'
]
