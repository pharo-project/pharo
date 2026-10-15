Class {
	#name : 'ClapDbgHelpCommand',
	#superclass : 'ClapDbgCommand',
	#category : 'Debugger-CLI-Commands',
	#package : 'Debugger-CLI',
	#tag : 'Commands'
}

{ #category : 'execution' }
ClapDbgHelpCommand >> basicExecuteOn: aDebugger with: args [

	| helpArray informLine |
	helpArray := aDebugger commands values collect: [ :command | String tab , command printHelp ].
	informLine := '=========== available commands ==========='.

	^ informLine , aDebugger lineEnding , (helpArray asSet asArray joinUsing: aDebugger lineEnding)
]

{ #category : 'initialization' }
ClapDbgHelpCommand >> initialize [

	super initialize.
	self commandName: #( 'help' 'h' ).
	self commandHelp: 'print this help'.
	self arguments: ''
]
