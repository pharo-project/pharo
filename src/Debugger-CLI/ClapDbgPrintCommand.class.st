Class {
	#name : 'ClapDbgPrintCommand',
	#superclass : 'ClapDbgCommand',
	#category : 'Debugger-CLI',
	#package : 'Debugger-CLI'
}

{ #category : 'execution' }
ClapDbgPrintCommand >> executeOn: aDebugger with: args [

	| evaluation |
	args trim ifEmpty: [ ^ 'Nothing to print' ].
	[
		evaluation := OpalEvaluator new
			              context: aDebugger context;
			              evaluate: args.

		^ evaluation asString ]
		on: Error
		do: [ :ex |
				| indexOfDoIt allLines usefulLines |
				allLines := ex stackTraceString lines.
				indexOfDoIt := allLines findFirst: [ :l | l includesSubstring: 'DoIt' ].
				usefulLines := indexOfDoIt > 0
					               ifTrue: [ allLines copyFrom: 1 to: indexOfDoIt ]
					               ifFalse: [ allLines ].
				^ usefulLines joinUsing: aDebugger lineEnding ]
]

{ #category : 'initialization' }
ClapDbgPrintCommand >> initialize [

	super initialize.
	self commandName: #( 'print' 'p' ).
	self commandHelp: 'evaluate the expression'.
	self arguments: '<expression>'
]
