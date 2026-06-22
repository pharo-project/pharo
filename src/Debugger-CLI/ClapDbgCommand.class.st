Class {
	#name : 'ClapDbgCommand',
	#superclass : 'Object',
	#instVars : [
		'commandName',
		'commandHelp',
		'arguments'
	],
	#category : 'Debugger-CLI',
	#package : 'Debugger-CLI'
}

{ #category : 'accessing' }
ClapDbgCommand >> arguments [

	^ arguments
]

{ #category : 'accessing' }
ClapDbgCommand >> arguments: anObject [

	arguments := anObject
]

{ #category : 'accessing' }
ClapDbgCommand >> commandHelp [

	^ commandHelp
]

{ #category : 'accessing' }
ClapDbgCommand >> commandHelp: anObject [

	commandHelp := anObject
]

{ #category : 'accessing' }
ClapDbgCommand >> commandName [

	^ commandName
]

{ #category : 'accessing' }
ClapDbgCommand >> commandName: anObject [

	commandName := anObject
]

{ #category : 'execution' }
ClapDbgCommand >> executeOn: aDebugger with: args [ 
	^ self subclassResponsibility
]

{ #category : 'printing' }
ClapDbgCommand >> printHelp [

	^ '{1} ( or ''{2}'') {3} : {4}' format: {
			  (commandName first).
			  (commandName last).
			  (arguments).
			  commandHelp }
]
