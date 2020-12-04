"
I represent a the result of a success task.

I ignore failure callbacks and schedule for execution success callbacks
"
Class {
	#name : #TKTFutureSuccess,
	#superclass : #TKTFutureState,
	#instVars : [
		'value'
	],
	#category : #'TaskIt-Futures'
}

{ #category : #accessing }
TKTFutureSuccess >> basicValue [

	^ value
]

{ #category : #testing }
TKTFutureSuccess >> isSuccess [

	^ true
]

{ #category : #callbacks }
TKTFutureSuccess >> onSuccessDo: aBlockClosure inFuture: aFuture [
	
	aFuture scheduleCallbackTask: [ aBlockClosure value: value ]
]

{ #category : #accessing }
TKTFutureSuccess >> value [

	^ value
]

{ #category : #accessing }
TKTFutureSuccess >> value: aValue [

	value := aValue
]

{ #category : #accessing }
TKTFutureSuccess >> valueForFuture: aFuture [

	^ value
]
