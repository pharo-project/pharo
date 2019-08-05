"
Testing the DebugSession>isLatestContext: method
"
Class {
	#name : #IsLatestContextTest,
	#superclass : #DebuggerTest,
	#category : #'Debugger-Tests'
}

{ #category : #helper }
IsLatestContextTest >> stepA1 [
	self stepA2
]

{ #category : #helper }
IsLatestContextTest >> stepA2 [
]

{ #category : #tests }
IsLatestContextTest >> testIsLatestContextTest [
	self settingUpSessionAndProcessAndContextForBlock: [ self stepA1 ].
	"Checking that a Context that has nothing to do with the debugged execution is not LatestContext"
	self assert: (session isLatestContext: [] asContext) not.
	
	"Checking that context (the starting context of the execution) is the LatestContext until a method is called"
	self assert: (session isLatestContext: context).
	session stepInto.
	self assert: (session isLatestContext: context).
	session stepInto.
	self assert: (session isLatestContext: context) not.
	session stepInto.
	self assert: (session isLatestContext: context) not.
	session stepInto.
	"The called method returned. context is LatestContext again."
	self assert: (session isLatestContext: context).
]
