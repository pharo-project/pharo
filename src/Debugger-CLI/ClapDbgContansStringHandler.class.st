"
""Handles the command !?<aString> and return (if it exists) the most recent typed contaning aString""
"
Class {
	#name : 'ClapDbgContansStringHandler',
	#superclass : 'ClapDbgDebuggerHistoryHandler',
	#category : 'Debugger-CLI',
	#package : 'Debugger-CLI'
}

{ #category : 'execution' }
ClapDbgContansStringHandler >> canHandle: aString [
	
	^ aString beginsWith: '!?'
]

{ #category : 'execution' }
ClapDbgContansStringHandler >> handle: aString for: aDebugger [

	| unMot commandsArray |
	unMot := aString copyFrom: 3 to: aString size.

	commandsArray := aDebugger commandsStack asArray.
	^ commandsArray detect: [ :command | command includesSubstring: unMot ] ifNone: [ ClapDbgHistoryEventNotFound signal: aString , ': event not found' ]
]
