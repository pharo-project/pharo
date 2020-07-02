"
I represent the state of a future. A future can be in three different states represented by my subclasses:

 - Not yet finished
 - Success
 - Failure

A future starts in a Not yet finished state and goes either to the Failure or Success state depending on how the executed task finishes.
I am a stateless implementation of the state pattern. Thus, my messages receive the corresponding future as argument.

My main messages are:

 - #isFinished 
    testing if a future is indeed finished
 - onSuccessDo:inFuture:
    scheduling a callback in the given future in case of success
 - onFailureDo:inFuture:
    scheduling a callback in the given future in case of failure
"
Class {
	#name : #TKTFutureState,
	#superclass : #Object,
	#category : #'TaskIt-Futures'
}

{ #category : #testing }
TKTFutureState >> isFailure [

	^ false
]

{ #category : #testing }
TKTFutureState >> isFinished [
	
	^ true
]

{ #category : #testing }
TKTFutureState >> isSuccess [

	^ false
]

{ #category : #callbacks }
TKTFutureState >> onFailureDo: aBlockClosure inFuture: aFuture [

	"nothing"
]

{ #category : #callbacks }
TKTFutureState >> onSuccessDo: aBlockClosure inFuture: aFuture [
	
	"nothing"
]
