"
I am a callback error handler that forks a process with the callback error.
I execute the given block and handle it with #on:fork:.

I collaborate with TFRunner instances that centralize the callback execution and return per runner.
"
Class {
	#name : #TFForkCallbackExceptionHandler,
	#superclass : #Object,
	#category : #'ThreadedFFI-Callbacks'
}

{ #category : #errors }
TFForkCallbackExceptionHandler >> handleExceptionDuring: aBlock [

	aBlock on: Exception fork: [ :e | e pass ]
]
