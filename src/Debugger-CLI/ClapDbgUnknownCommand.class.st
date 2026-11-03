Class {
	#name : 'ClapDbgUnknownCommand',
	#superclass : 'ClapDbgCommand',
	#instVars : [
		'command'
	],
	#category : 'Debugger-CLI-Commands',
	#package : 'Debugger-CLI',
	#tag : 'Commands'
}

{ #category : 'execution' }
ClapDbgUnknownCommand >> basicExecuteOn: aDebugger with: args [

	^ 'Unknown command: ' , command
]

{ #category : 'accessing' }
ClapDbgUnknownCommand >> command [

	^ command
]

{ #category : 'accessing' }
ClapDbgUnknownCommand >> command: anObject [

	command := anObject
]

{ #category : 'initialization' }
ClapDbgUnknownCommand >> initialize [

	super initialize.
	self commandName: #( 'any other command' 'anUnknownCommand' ).
	self commandHelp: 'Unknown command'.
	self arguments: ''
]
