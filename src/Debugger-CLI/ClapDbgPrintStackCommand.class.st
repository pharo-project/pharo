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

	| results remainingStackText firstOfStack |
	results := aDebugger context contextStack collect: [ :ctx | ctx asString ].

	firstOfStack := '------> ' , results first asString.
	remainingStackText := (results allButFirst collect: [ :res | String tab , res ]) joinUsing: aDebugger lineEnding.
	^ '==================== Call Stack ====================' , aDebugger lineEnding , firstOfStack , aDebugger lineEnding
	  , remainingStackText
]

{ #category : 'initialization' }
ClapDbgPrintStackCommand >> initialize [

	super initialize.
	self commandName: #( 'printStack' 'ps' ).
	self commandHelp: 'print the current Stack'.
	self arguments: ''
]
