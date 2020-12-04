"
! TKTFuture

In TaskIT we differentiate two different kind of tasks: some tasks are just scheduled for execution, they produce some side-effect and no result, some other tasks will produce (generally) a side-effect free value. When the result of a task is important for us, TaskIT provides us with a future object. A future is no other thing than an object that represents the future value of the task's execution. We can schedule a task with a future by using the future message on a block closure, as follows.

[[[language=smalltalk
aFuture := [ 2 + 2 ] future.
]]]

One way to see futures is as placeholders. When the task is finished, it deploys its result into the corresponding future. A future then provides access to its value, but since we cannot know when this value will be available, we cannot access it right away. Instead, futures provide an asynchronous way to access it's value by using callbacks. A callback is an object that will be executed when the task execution is finished.

In general terms, we do not want to force a future to retrieve his value in a synchronous way. By doing so, we would be going back to the synchronous world, blocking a process' execution, and not exploiting concurrency. Later sections will discuss about synchronous (blocking) retrieval of a future's value.
A future can provide two kind of results: either the task execution was a success or a failure. A success happens when the task completes in a normal way, while a failure happens when an uncatched exception is risen in the task. Because of these distinction, futures allow the subscription of two different callbacks using the methods onSuccessDo: and onFailureDo:.

In the example below, we create a future and subscribe to it a success callback. As soon as the task finishes, the value gets deployed in the future and the callback is called with it.

[[[language=smalltalk
aFuture := [ 2 + 2 ] future.
aFuture onSuccessDo: [ :result | result logCr ].
]]]

We can also subscribe callbacks that handle a task's failure using the onFailureDo: message. If an exception occurs and the task cannot finish its execution as expected, the corresponding exception will be passed as argument to the failure callback, as in the following example.

[[[language=smalltalk
aFuture := [ Error signal ] future.
aFuture onFailureDo: [ :error | error sender method selector logCr ].
]]]

Futures accept more than one callback. When its associated task is finished, all its callbacks will be scheduled for execution. In other words, the only guarantee that callbacks give us is that they will be all eventually executed. However, the future itself cannot guarantee neither when will the callbacks be executed, nor in which order. The following example shows how we can subscribe several success callbacks for the same future.

[[[language=smalltalk
future := [ 2 + 2 ] future.
future onSuccessDo: [ :v | FileStream stdout nextPutAll: v asString; cr ].
future onSuccessDo: [ :v | 'Finished' logCr ].
future onSuccessDo: [ :v | [ v factorial logCr ] schedule ].
future onFailureDo: [ :error | error logCr ].
]]]

Callbacks work wether the task is still running or already finished. If the task is running, callbacks are registered and wait for the completion of the task. If the task is already finished, the callback will be immediately scheduled with the already deployed value. See below a code examples that illustrates this: we first create a future and subscribes a callback before it is finished, then we wait for its completion and subscribe a second callback afterwards. Both callbacks are scheduled for execution.

[[[language=smalltalk
future := [ 1 second wait. 2 + 2 ] future.
future onSuccessDo: [ :v | v logCr ].

2 seconds wait.
future onSuccessDo: [ :v | v logCr ].
]]]

!! Combinators

Futures are a nice asynchronous way to obtain the results of our eventually executed tasks. However, as we do not know when tasks will finish, processing that result will be another asynchronous task that needs to start as soon as the first one finishes. To simplify the task of future management, TaskIT futures come along with some combinators.

!!! Value combinators

- The collect: combinator
The collect: combinator does, as its name says, the same than the collection's API: it transforms a result using a transformation.

[[[language=smalltalk
future := [ 2 + 3 ] future.
(future collect: [ :number | number factorial ])
    onSuccessDo: [ :result | result logCr ].
]]]

The collect: combinator returns a new future whose value will be the result of transforming the first future's value.

- The select: combinator
The select: combinator does, as its name says, the same than the collection's API: it filters a result satisfying a condition.

[[[language=smalltalk
future := [ 2 + 3 ] future.
(future select: [ :number | number even ])
    onSuccessDo: [ :result | result logCr ];
    onFailureDo: [ :error | error logCr ].
]]]

The select: combinator returns a new future whose result is the result of the first future if it satisfies the condition. Otherwise, its value will be a NotFound exception.

- The flatCollect:combinator
The flatCollect: combinator is similar to the collect: combinator, as it transforms the result of the first future using the given transformation block. However, flatCollect: excepts as the result of its transformation block a future.

[[[language=smalltalk
future := [ 2 + 3 ] future.
(future flatCollect: [ :number | [ number factorial ] future ])
    onSuccessDo: [ :result | result logCr ].
The flatCollect: combinator returns a new future whose value will be the result the value of the future yielded by the transformation.
]]]

- The zip:combinator
The zip: combinator combines two futures into a single future that returns an array with both results.

[[[language=smalltalk
future1 := [ 2 + 3 ] future.
future2 := [ 18 factorial ] future.
(future1 zip: future2)
    onSuccessDo: [ :result | result logCr ].
]]]

zip: works only on success: the resulting future will be a failure if any of the futures is also a failure.

- The on:do:combinator
The on:do: allows us to transform a future that fails with an exception into a future with a result.

[[[language=smalltalk
future := [ Error signal ] future
    on: Error do: [ :error | 5 ].
future onSuccessDo: [ :result | result logCr ].
]]]

!!! Synchronization Combinators

- The fallbackTo: combinator
The fallbackTo: combinator combines two futures in a way such that if the first future fails, it is the second one that will be taken into account.

[[[language=smalltalk
failFuture := [ Error signal ] future.
successFuture := [ 1 + 1 ] future.
(failFuture fallbackTo: successFuture)
    onSuccessDo: [ :result | result logCr ].
]]]

In other words, fallbackTo: produces a new future whose value is the first's future value if success, or it is the second future's value otherwise.

- The firstCompleteOf: combinator
The firstCompleteOf: combinator combines two futures resulting in a new future whose value is the value of the future that finishes first, wether it is a success or a failure.

[[[language=smalltalk
failFuture := [ 1 second wait. Error signal ] future.
successFuture := [ 1 second wait. 1 + 1 ] future.
(failFuture firstCompleteOf: successFuture)
    onSuccessDo: [ :result | result logCr ];
    onFailureDo: [ :error | error logCr ].
]]]

In other words, fallbackTo: produces a new future whose value is the first's future value if success, or it is the second future's value otherwise.

- The andThen: combinator
The andThen: combinator allows to chain several futures to a single future's value. All futures chained using the andThen: combinator are guaranteed to be executed sequenceally (in contrast to normal callbacks), and all of them will receive as value the value of the first future (instead of the of of it's preceeding future).

[[[language=smalltalk
[ 1 + 1 ] future
    andThen: [ :result | result logCr ];
    andThen: [ :result | FileStream stdout nextPutAll: result ]. 
]]]

This combinator is meant to enforce the order of execution of several actions, and this it is mostly for side-effect purposes where we want to guarantee such order.
"
Class {
	#name : #TKTFuture,
	#superclass : #Object,
	#instVars : [
		'result',
		'valueSemaphore',
		'resultCallbacks',
		'exceptionCallbacks',
		'runner'
	],
	#category : #'TaskIt-Futures'
}

{ #category : #'instance creation' }
TKTFuture class >> doing: aBlock [

	^ TKTConfiguration runner future: aBlock
]

{ #category : #'instance creation' }
TKTFuture class >> fromCollectionOfFutures: aCollectionOfFutures [

	^ self
		reduceCollectionOfFutures: aCollectionOfFutures
		with: [ :acum :each | acum add: each. acum ] 
]

{ #category : #'instance creation' }
TKTFuture class >> of: aValue [

	^ self new deploySuccess: aValue
]

{ #category : #'instance creation' }
TKTFuture class >> reduceCollectionOfFutures: aCollectionOfFutures with: aBlock [

	^ aCollectionOfFutures
		inject: (TKTFuture of: OrderedCollection new)
		into: [ :facum :feach | facum flatCollect: [ :acum | 
				feach collect: [ :each | aBlock value: acum value: each ] ] ]
]

{ #category : #callbacks }
TKTFuture >> addFailureCallback: aBlockClosure [

	exceptionCallbacks add: aBlockClosure
]

{ #category : #callbacks }
TKTFuture >> addSuccessCallback: aBlockClosure [ 
	
	resultCallbacks add: aBlockClosure
]

{ #category : #combinators }
TKTFuture >> andThen: aBlockClosure [ 
	
	| future |
	future := TKTFuture new.
	self onSuccessDo: [ :got | 
		[ aBlockClosure value: got ]
			on: Error do: [ :e | "Nothing?" ].
		future deploySuccess: got ].
	self onFailureDo: [ :error | future deployFailure: error freeze ].
	^ future
]

{ #category : #combinators }
TKTFuture >> collect: aBlockClosure [
	| future |
	future := TKTFuture new.
	future runner: runner.
	self
		onSuccessDo: [ :got | 
			[ future deploySuccess: (aBlockClosure value: got) ]
				on: Error
				do: [ :e | future deployFailure: e freeze ] ].
	self onFailureDo: [ :error | future deployFailure: error freeze ].
	^ future
]

{ #category : #deployment }
TKTFuture >> deployFailure: aValue [
	self installResult: (TKTFutureFailure new value: aValue).
	exceptionCallbacks
		do: [ :each | self scheduleCallbackTask: [ each value: aValue ] ].
	valueSemaphore signal
]

{ #category : #deployment }
TKTFuture >> deploySuccess: aValue [
	self installResult: (TKTFutureSuccess new value: aValue).
	resultCallbacks
		do: [ :each | self scheduleCallbackTask: [ each value: aValue ] ].
	valueSemaphore signal
]

{ #category : #combinators }
TKTFuture >> fallbackTo: aFallbackFuture [ 
	
	^ self recoverWith: [ :error | aFallbackFuture ]
]

{ #category : #combinators }
TKTFuture >> firstCompleteOf: anotherFuture [
	
	| finished future |
	finished := false.
	future := TKTFuture new.
	future runner: runner.
	self onSuccessDo: [ :v |
		finished ifFalse: [
			finished := true.
			future deploySuccess: v ]].
	self onFailureDo: [ :v |
		finished ifFalse: [
			finished := true.
			future deployFailure: v freeze ]].
	anotherFuture onSuccessDo: [ :v |
		finished ifFalse: [
			finished := true.
			future deploySuccess: v ]].
	anotherFuture onFailureDo: [ :v |
		finished ifFalse: [
			finished := true.
			future deployFailure: v freeze ]].
	^ future
]

{ #category : #combinators }
TKTFuture >> flatCollect: aBlockClosure [ 
	
	| future |
	future := TKTFuture new.
	future runner: runner.
	self onSuccessDo: [ :got | [(aBlockClosure value: got)
		onSuccessDo: [ :got2 | future deploySuccess: got2 ];
		onFailureDo: [ :error | future deployFailure: error freeze ]]
				on: Error do: [ :e | future deployFailure: e freeze ] ].
	self onFailureDo: [ :error | future deployFailure: error freeze ].
	^ future
]

{ #category : #initialization }
TKTFuture >> initialize [

	super initialize.
	result := TKTFutureNotFinished new.
	runner := TKTConfiguration runner.

	valueSemaphore := Semaphore new.

	exceptionCallbacks := OrderedCollection new.
	resultCallbacks := OrderedCollection new.
]

{ #category : #private }
TKTFuture >> installResult: aResult [

	result isFinished
		ifTrue: [ self error: 'cannot deploy twice in same future' ].
	result := aResult
]

{ #category : #testing }
TKTFuture >> isFailure [

	^ result isFailure
]

{ #category : #testing }
TKTFuture >> isFinished [

	^ result isFinished
]

{ #category : #testing }
TKTFuture >> isSuccess [

	^ result isSuccess
]

{ #category : #combinators }
TKTFuture >> on: anError do: aBlockClosure [ 
	
	| future |
	future := TKTFuture new.
	future runner: runner.
	self onSuccessDo: [ :got | future deploySuccess: got ].
	self onFailureDo: [ :error | 
		(anError handles: error)
			ifTrue: [ [future deploySuccess: (aBlockClosure value: error)]
							on: Error do: [ :e | future deployFailure: e freeze ]  ]
			ifFalse: [ future deployFailure: error freeze ] ].
	^ future
]

{ #category : #callbacks }
TKTFuture >> onFailureDo: aBlockClosure [ 
	
	result onFailureDo: aBlockClosure inFuture: self
]

{ #category : #callbacks }
TKTFuture >> onSuccessDo: aCallback [

	result onSuccessDo: aCallback inFuture: self
]

{ #category : #printing }
TKTFuture >> printOn: aStream [

	super printOn: aStream.
	aStream
		nextPutAll: '(finished=';
		nextPutAll: self isFinished asString;
		nextPutAll: ';value=';
		nextPutAll: result asString;
		nextPutAll: ')'
]

{ #category : #combinators }
TKTFuture >> recoverWith: aBlock [
	
	| future |
	future := TKTFuture new.
	future runner: runner.
	self onSuccessDo: [ :value | future deploySuccess: value ].
	self onFailureDo: [ :error | | recoverFuture |
		recoverFuture := aBlock value: error.
		recoverFuture onSuccessDo: [ :value | future deploySuccess: value ].
		recoverFuture onFailureDo: [ :value | future deployFailure: value freeze ] ].	
	^ future
]

{ #category : #accessing }
TKTFuture >> runner: aRunner [

	runner := aRunner
]

{ #category : #callbacks }
TKTFuture >> scheduleCallbackTask: aCallbackTask [
	runner schedule: aCallbackTask
]

{ #category : #combinators }
TKTFuture >> select: aBlockClosure [ 
	
	^ self collect: [ :r | (aBlockClosure value: r) ifTrue: [ r ] ifFalse: [ NotFound signalFor: aBlockClosure ] ].
	
]

{ #category : #synchronization }
TKTFuture >> synchronizeTimeout: aTimeout [
	
	self waitForCompletion: aTimeout.
	^ result valueForFuture: self
]

{ #category : #synchronization }
TKTFuture >> waitForCompletion: aTimeout [

	self isFinished ifFalse: [ | expired |
		expired := valueSemaphore waitTimeoutMSecs: aTimeout asMilliSeconds.
		expired ifTrue: [ TKTTimeoutException signal ] ]
]

{ #category : #combinators }
TKTFuture >> zip: secondFuture [
	
	^ self zip: secondFuture with: [ :a :b | Array with: a with: b ]
]

{ #category : #combinators }
TKTFuture >> zip: secondFuture with: aBlock [
	
	^ self flatCollect: [ :result1 | secondFuture collect: [ :result2 | aBlock value: result1 value: result2 ] ]
]
