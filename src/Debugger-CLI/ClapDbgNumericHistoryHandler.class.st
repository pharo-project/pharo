"
Handles command !<anInteger> and returns (if it exists) the command related to the number.

Syntax is:
- `!-2` returns the second last command
- `!2` returns the second command
"
Class {
	#name : 'ClapDbgNumericHistoryHandler',
	#superclass : 'ClapDbgDebuggerHistoryHandler',
	#category : 'Debugger-CLI-Core',
	#package : 'Debugger-CLI',
	#tag : 'Core'
}

{ #category : 'execution' }
ClapDbgNumericHistoryHandler >> canHandle: aString [

	| rest |
	(aString beginsWith: '!') ifFalse: [ ^ false ].

	rest := aString allButFirst.
	^ rest startsWithDigit or: [ (rest beginsWith: '-') and: [ rest allButFirst startsWithDigit ] ]
]

{ #category : 'execution' }
ClapDbgNumericHistoryHandler >> handle: aString for: aClapDebugger [
	"aString is the text entered by the user.
	It should start with !
	Then it expects either a positive or negative number"
	
	| indexString targetIndex commandsArray |
	indexString := aString allButFirst.
	targetIndex := indexString asInteger.
	targetIndex isInteger ifFalse: [ ^ aString ]. 
	commandsArray := aClapDebugger commandsStack asArray.

	targetIndex < 0 
		ifTrue: [ ^ commandsArray 
						at: targetIndex abs 
						ifAbsent: [ ClapDbgHistoryEventNotFound signal: aString , ': event not found' ] ].
	targetIndex > 0 
		ifTrue: [ ^ commandsArray 
			at: commandsArray size - targetIndex + 1  
			ifAbsent: [ ClapDbgHistoryEventNotFound signal: aString , ': event not found' ] ].
	^ aString
]
