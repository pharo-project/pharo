Class {
	#name : #TFCallbackForkRunStrategy,
	#superclass : #Object,
	#category : #'ThreadedFFI-Callbacks'
}

{ #category : #executing }
TFCallbackForkRunStrategy >> executeCallback: aCallbackInvocation on: aTFRunner [

	[ aTFRunner handleExceptionDuring: [ aCallbackInvocation execute ] ]
		forkAt: Processor highIOPriority - 1
]
