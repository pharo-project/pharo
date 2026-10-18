"
I handle commands starting with !! and 
return the last command typed.


"
Class {
	#name : 'ClapDbgLastCommandHandler',
	#superclass : 'ClapDbgDebuggerHistoryHandler',
	#category : 'Debugger-CLI-Core',
	#package : 'Debugger-CLI',
	#tag : 'Core'
}

{ #category : 'execution' }
ClapDbgLastCommandHandler >> canHandle: aString [

	^ (aString beginsWith: '!!') or: [ aString isEmpty ]
]

{ #category : 'execution' }
ClapDbgLastCommandHandler >> handle: aString for: aDebugger [
	"aString should start with !!"
	
	^ aDebugger lastCommand , (aString copyReplaceAll: '!' with: '')
]
