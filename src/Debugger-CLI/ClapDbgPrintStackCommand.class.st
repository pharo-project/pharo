Class {
	#name : 'ClapDbgPrintStackCommand',
	#superclass : 'ClapDbgCommand',
	#category : 'Debugger-CLI',
	#package : 'Debugger-CLI'
}

{ #category : 'execution' }
ClapDbgPrintStackCommand >> executeOn: aDebugger [

	^ self executeOn: aDebugger with: ''
]

{ #category : 'execution' }
ClapDbgPrintStackCommand >> executeOn: aDebugger with: args [

	| stackText prefix currentContext |
	currentContext := aDebugger process suspendedContext.
	stackText := '==================== Call Stack ====================' , aDebugger lineEnding.

	[ currentContext isNil ] whileFalse: [
			prefix := currentContext == aDebugger context
				          ifTrue: [ '------> ' ]
				          ifFalse: [ '        ' ].

			stackText := stackText , prefix , currentContext printString , aDebugger lineEnding.

			currentContext := currentContext sender ].

	^ stackText
]

{ #category : 'initialization' }
ClapDbgPrintStackCommand >> initialize [

	super initialize.
	self commandName: #( 'printStack' 'ps' ).
	self commandHelp: 'print the current Stack'.
	self arguments: ''
]
