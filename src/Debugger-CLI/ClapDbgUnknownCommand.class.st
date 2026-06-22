Class {
	#name : 'ClapDbgUnknownCommand',
	#superclass : 'ClapDbgCommand',
	#instVars : [
		'command'
	],
	#category : 'Debugger-CLI',
	#package : 'Debugger-CLI'
}

{ #category : 'accessing' }
ClapDbgUnknownCommand >> command [

	^ command
]

{ #category : 'accessing' }
ClapDbgUnknownCommand >> command: anObject [

	command := anObject
]

{ #category : 'printing' }
ClapDbgUnknownCommand >> executeOn: aDebugger with: args [

	^ 'Unknown command: ' , command
]

{ #category : 'initialization' }
ClapDbgUnknownCommand >> initialize [

	super initialize.
	self commandName: #( 'any other command' 'anUnknownCommand' ).
	self commandHelp: 'Unknown command'.
	self arguments: ''
]
