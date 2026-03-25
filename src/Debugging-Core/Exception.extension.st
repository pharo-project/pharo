Extension { #name : 'Exception' }

{ #category : '*Debugging-Core' }
Exception >> completeProcess: aProcess with: aContext [
	"I am a hook during process completion in case one of my subclass wants to plug a special behavior.

	By default I return my signal context"

	^ self signalContext
]

{ #category : '*Debugging-Core' }
Exception >> retry [
	"Abort an exception handler and re-evaluate its protected block."

	handlerContext restart
]

{ #category : '*Debugging-Core' }
Exception >> retryUsing: alternativeBlock [
	"Abort an exception handler and evaluate a new block in place of the handler's protected block."

	handlerContext restartWithNewReceiver: alternativeBlock
]
