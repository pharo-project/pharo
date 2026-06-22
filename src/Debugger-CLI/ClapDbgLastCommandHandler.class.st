"
""Handles the command !! and return the most recent command typed ""
"
Class {
	#name : 'ClapDbgLastCommandHandler',
	#superclass : 'ClapDbgDebuggerHistoryHandler',
	#category : 'Debugger-CLI',
	#package : 'Debugger-CLI'
}

{ #category : 'testing' }
ClapDbgLastCommandHandler >> canHandle: aString [

	^ aString beginsWith: '!!'
]

{ #category : 'testing' }
ClapDbgLastCommandHandler >> handle: aString for: aDebugger [
    ^ aDebugger lastCommand , (aString allButFirst: 2)
]
