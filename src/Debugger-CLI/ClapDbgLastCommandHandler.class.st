"
""Handles the command !! and return the most recent command typed ""
"
Class {
	#name : 'ClapDbgLastCommandHandler',
	#superclass : 'ClapDbgDebuggerHistoryHandler',
	#category : 'Debugger-CLI',
	#package : 'Debugger-CLI'
}

{ #category : 'execution' }
ClapDbgLastCommandHandler >> canHandle: aString [

	^ (aString beginsWith: '!!') or: [ aString isEmpty ]
]

{ #category : 'execution' }
ClapDbgLastCommandHandler >> handle: aString for: aDebugger [

	^ aDebugger lastCommand , (aString copyReplaceAll: '!' with: '')
]
