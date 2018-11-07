Extension { #name : #Exception }

{ #category : #'*Debugging-Core' }
Exception >> retry [
	"Abort an exception handler and re-evaluate its protected block."

	handlerContext restart
]

{ #category : #'*Debugging-Core' }
Exception >> retryUsing: alternativeBlock [
	"Abort an exception handler and evaluate a new block in place of the handler's protected block."

	handlerContext restartWithNewReceiver: alternativeBlock

]
