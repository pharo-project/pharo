"
""Handles command !<anInteger> and return (if it exists) the command related to the number
	ex : !-2 returns the second last command
		  !2 returns the second command ""
"
Class {
	#name : 'ClapDbgNumericHistoryHandler',
	#superclass : 'ClapDbgDebuggerHistoryHandler',
	#category : 'Debugger-CLI',
	#package : 'Debugger-CLI'
}

{ #category : 'execution' }
ClapDbgNumericHistoryHandler >> canHandle: aString [

	| rest |
	(aString beginsWith: '!') ifFalse: [ ^ false ].

	rest := aString allButFirst.
	^ rest startsWithDigit or: [ (rest beginsWith: '-') and: [ rest allButFirst startsWithDigit ] ]
]

{ #category : 'execution' }
ClapDbgNumericHistoryHandler >> handle: aString for: aDebugger [

	| indexString targetIndex commandsArray |
	indexString := aString allButFirst.

	targetIndex := indexString asInteger.
	targetIndex isInteger ifFalse: [ ^ aString ]. "checking if it is a number. If not, return the string typed by the user"
	commandsArray := aDebugger commandsStack asArray.

	targetIndex < 0 ifTrue: [ ^ commandsArray at: targetIndex abs ifAbsent: [ ClapDbgHistoryEventNotFound signal: aString , ': event not found' ] ].

	targetIndex > 0 ifTrue: [ ^ commandsArray at: commandsArray size - targetIndex + 1  ifAbsent: [ ClapDbgHistoryEventNotFound signal: aString , ': event not found' ] ].

	^ aString
]
