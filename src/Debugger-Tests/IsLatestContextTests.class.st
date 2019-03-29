"
Testing the DebugSession>isLatestContext: method
"
Class {
	#name : #IsLatestContextTests,
	#superclass : #DebuggerTests,
	#category : #'Debugger-Tests'
}

{ #category : #helper }
IsLatestContextTests >> stepA1 [
	self stepA2
]

{ #category : #helper }
IsLatestContextTests >> stepA2 [
]

{ #category : #tests }
IsLatestContextTests >> testIsLatestContextTest [
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
