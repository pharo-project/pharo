Class {
	#name : 'ClapDbgHistoryCommand',
	#superclass : 'ClapDbgCommand',
	#category : 'Debugger-CLI',
	#package : 'Debugger-CLI'
}

{ #category : 'printing' }
ClapDbgHistoryCommand >> executeOn: aDebugger with: args [

	| lines |
	lines := aDebugger commandsStack asArray reversed withIndexCollect: [ :command :index | index asString , ' : ' , command ].
	^ aDebugger lineEnding join: lines
]

{ #category : 'initialization' }
ClapDbgHistoryCommand >> initialize [

	super initialize.
	self commandName: #( 'history' 'hist' ).
	self commandHelp: 'Give the history of last commands'.
	self arguments: ''
]
