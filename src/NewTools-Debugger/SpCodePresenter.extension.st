Extension { #name : #SpCodePresenter }

{ #category : #'*NewTools-Debugger' }
SpCodePresenter >> beForContext: aContext [
	self interactionModel: (StDebuggerContextInteractionModel on: aContext)
]
