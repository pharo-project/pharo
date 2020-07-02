"
I represent a not yet finished future. Not yet finished futures cannot really schedule callbacks: they will add the callbacks in the given future to be executed when the value is deployed.
"
Class {
	#name : #TKTFutureNotFinished,
	#superclass : #TKTFutureState,
	#category : #'TaskIt-Futures'
}

{ #category : #testing }
TKTFutureNotFinished >> isFinished [

	^ false
]

{ #category : #callbacks }
TKTFutureNotFinished >> onFailureDo: aBlockClosure inFuture: aFuture [
	
	aFuture addFailureCallback: aBlockClosure
]

{ #category : #callbacks }
TKTFutureNotFinished >> onSuccessDo: aBlockClosure inFuture: aFuture [
	
	aFuture addSuccessCallback: aBlockClosure
]
