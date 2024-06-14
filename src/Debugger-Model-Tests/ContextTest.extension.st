Extension { #name : 'ContextTest' }

{ #category : '*Debugger-Model-Tests' }
ContextTest >> testActivateReturnValue [
	self assert: ((aSender activateReturn: aMethodContext value: #()) isKindOf: Context).
	self assert: (aSender activateReturn: aMethodContext value: #()) receiver equals: aMethodContext
]
