"
I represent a the result of a failured task.

I ignore success callbacks and schedule for execution failure callbacks
"
Class {
	#name : #TKTFutureFailure,
	#superclass : #TKTFutureState,
	#instVars : [
		'value'
	],
	#category : #'TaskIt-Futures'
}

{ #category : #testing }
TKTFutureFailure >> isFailure [

	^ true
]

{ #category : #accessing }
TKTFutureFailure >> onFailureDo: aBlockClosure inFuture: aFuture [
	
	aFuture scheduleCallbackTask: [ aBlockClosure value: value ]
]

{ #category : #accessing }
TKTFutureFailure >> value [
	
	value signal
]

{ #category : #accessing }
TKTFutureFailure >> value: aValue [

	value := aValue
]

{ #category : #accessing }
TKTFutureFailure >> valueForFuture: aFuture [
	
	 value signal
]
