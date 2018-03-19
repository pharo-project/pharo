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

{ #category : #'*Debugging-Core' }
Exception >> return [
	"Return nil as the value of the block protected by the active exception handler."

	self return: self defaultReturnValue
]

{ #category : #'*Debugging-Core' }
Exception >> return: returnValue [
	"Return the argument as the value of the block protected by the active exception handler."

	handlerContext return: returnValue
]
