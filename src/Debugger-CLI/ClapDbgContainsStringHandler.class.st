"
Handles the command !?<aString> and return (if it exists) the most recently typed lines contaning aString.
"
Class {
	#name : 'ClapDbgContainsStringHandler',
	#superclass : 'ClapDbgDebuggerHistoryHandler',
	#category : 'Debugger-CLI-Core',
	#package : 'Debugger-CLI',
	#tag : 'Core'
}

{ #category : 'execution' }
ClapDbgContainsStringHandler >> canHandle: aString [
	
	^ aString beginsWith: '!?'
]

{ #category : 'execution' }
ClapDbgContainsStringHandler >> handle: aString for: aDebugger [
	"aString is expected to start with !? "
	
	| word commandsArray |
	word := aString copyFrom: 3 to: aString size.

	commandsArray := aDebugger commandsStack asArray.
	^ commandsArray 
		detect: [ :command | command includesSubstring: word ] 
		ifNone: [ ClapDbgHistoryEventNotFound signal: aString , ': event not found' ]
]
