Class {
	#name : 'ClapDbgHelpCommand',
	#superclass : 'ClapDbgCommand',
	#category : 'Debugger-CLI',
	#package : 'Debugger-CLI'
}

{ #category : 'printing' }
ClapDbgHelpCommand >> executeOn: aDebugger with: args [

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
