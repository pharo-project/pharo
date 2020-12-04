Extension { #name : #SpCodePresenter }

{ #category : #'*NewTools-Debugger' }
SpCodePresenter >> beForContext: aContext [
	self interactionModel: (StDebuggerContextInteractionModel on: aContext)
]

{ #category : #'*NewTools-Debugger' }
SpCodePresenter >> beForContextReceiver: aContext [

	self interactionModel: (StDebuggerContextReceiverInteractionModel on: aContext)
]
