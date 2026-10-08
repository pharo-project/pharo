"
""Handles the command !<aString> and return (if it exists) the most recent command typed starting with aString""
"
Class {
	#name : 'ClapDbgStartsWithStringHandler',
	#superclass : 'ClapDbgDebuggerHistoryHandler',
	#category : 'Debugger-CLI',
	#package : 'Debugger-CLI'
}

{ #category : 'execution' }
ClapDbgStartsWithStringHandler >> canHandle: aString [

	| rest |
	(aString beginsWith: '!!') ifTrue: [ ^ false ].
	(aString beginsWith: '!?') ifTrue: [ ^ false ].
	(aString beginsWith: '!') ifFalse: [ ^ false ].

	rest := aString allButFirst.
	
	"to prevent having just a '!'"
	rest isEmpty ifTrue: [ ^ false ].

	^ (rest startsWithDigit or: [ (rest beginsWith: '-') and: [ rest allButFirst startsWithDigit ] ]) not
]

{ #category : 'execution' }
ClapDbgStartsWithStringHandler >> handle: aString for: aDebugger [

	| unMot commandsArray |
	unMot := aString allButFirst.
	commandsArray := aDebugger commandsStack asArray.

	^ commandsArray detect: [ :command | command beginsWith: unMot ] ifNone: [ ClapDbgHistoryEventNotFound signal: aString , ': event not found' ]
]
