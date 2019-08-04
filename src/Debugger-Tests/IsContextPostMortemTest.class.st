Class {
	#name : #IsContextPostMortemTest,
	#superclass : #DebuggerTest,
	#category : #'Debugger-Tests'
}

{ #category : #helper }
IsContextPostMortemTest >> stepA1 [
	^ self stepA2
]

{ #category : #helper }
IsContextPostMortemTest >> stepA2 [
	^ 42
]

{ #category : #tests }
IsContextPostMortemTest >> testIsContextPostMortem [
	self settingUpSessionAndProcessAndContextForBlock: [ self stepA1 ].
	session stepInto.
	"The top context of the context chain is not post mortem"
	self assert: (session isContextPostMortem: session interruptedProcess suspendedContext) not.
	"A context from the context chain is not post mortem"
	self assert: (session isContextPostMortem: context) not.
	"A context not from the context chain is post mortem..."
	self assert: (session isContextPostMortem: [2] asContext).
	process suspendedContext: nil.
	"...unless the process' suspended context is nil, in which case, no context is post mortem"
	self assert: (session isContextPostMortem: [2] asContext) not.
]
