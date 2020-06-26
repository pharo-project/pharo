Class {
	#name : #TKTFutureTests,
	#superclass : #TKTTestCase,
	#category : #'TaskIt-Tests-Futures'
}

{ #category : #initialization }
TKTFutureTests >> setUp [
	super setUp.
	TKTConfiguration runner reset
]

{ #category : #'tests-callbacks' }
TKTFutureTests >> testCannotDeployFailureTwiceIntoFuture [

	| future |
	future := TKTFuture new.
	future deployFailure: Error new.
	self should: [future deployFailure: Error new] raise: Error.
]

{ #category : #'tests-callbacks' }
TKTFutureTests >> testCannotDeployTwiceIntoFuture [

	| future |
	future := TKTFuture new.
	future deploySuccess: 2.
	self should: [future deploySuccess: 3] raise: Error.
]

{ #category : #'tests-combinators' }
TKTFutureTests >> testFutureAndThenExecutesInOrder [

	| future collection |
	future := TKTFuture new.
	collection := OrderedCollection new.
	
	future
		andThen: [ :v | collection add: v + 1 ];
		andThen: [ :v | collection add: v + 2 ];
		andThen: [ :v | collection add: v + 3 ].

	future deploySuccess: 0.
	50 milliSecond wait.
	
	self assert: collection asArray equals: #( 1 2 3)
]

{ #category : #'tests-combinators' }
TKTFutureTests >> testFutureAndThenFailsIfFirstFails [
	| future collection thenFuture got error |
	future := TKTFuture new.
	collection := OrderedCollection new.
	thenFuture := future
		andThen: [ :v | collection add: v + 1 ];
		andThen: [ :v | collection add: v + 2 ];
		andThen: [ :v | collection add: v + 3 ].
	thenFuture onFailureDo: [ :e | got := e ].
	error := Error new.
	[ error signal ]
		on: Error
		do: [ :e | future deployFailure: e ].
	50 milliSecond wait.
	self assert: got equals: error
]

{ #category : #'tests-combinators' }
TKTFutureTests >> testFutureAndThenSuccessIfAndThenFails [

	| future collection thenFuture got |
	future := TKTFuture new.
	collection := OrderedCollection new.
	
	thenFuture := future
		andThen: [ :v | collection add: v + 1 ];
		andThen: [ :v | self error ];
		andThen: [ :v | collection add: v + 3 ].

	thenFuture onSuccessDo: [ :e | got := e ].
	
	future deploySuccess: 2.
	50 milliSecond wait.
	
	self assert: got equals: 2
]

{ #category : #'tests-combinators' }
TKTFutureTests >> testFutureAndThenThenFailuresAreJustSkipped [

	| future collection thenFuture |
	future := TKTFuture new.
	collection := OrderedCollection new.
	
	thenFuture := future
		andThen: [ :v | collection add: v + 1 ];
		andThen: [ :v | self error ];
		andThen: [ :v | collection add: v + 3 ].

	future deploySuccess: 2.
	50 milliSecond wait.
	
	self assert: collection asArray equals: #(3 5)
]

{ #category : #'tests-combinators' }
TKTFutureTests >> testFutureCollectFailsOnFailingMapFunction [
	| future got error |
	future := TKTFuture new.
	error := Error new.
	(future collect: [ :v | error signal ]) onFailureDo: [ :v | got := v ].
	future deploySuccess: 2.
	50 milliSecond wait.
	self assert: got equals: error
]

{ #category : #'tests-combinators' }
TKTFutureTests >> testFutureCollectFailsOnFailure [
	| future got error |
	future := TKTFuture new.
	(future collect: [ :v | v * 5 ]) onFailureDo: [ :v | got := v ].
	error := Error new.
	[ error signal ]
		on: Error
		do: [ :e | future deployFailure: e ].
	50 milliSecond wait.
	self assert: got equals: error
]

{ #category : #'tests-combinators' }
TKTFutureTests >> testFutureCollectSuccessOnSuccess [

	| future got |
	future := TKTFuture new.	

	(future collect: [ :v | v * 5 ]) onSuccessDo: [ :v | got := v ].
	
	future deploySuccess: 2.
	50 milliSecond wait.
	
	self assert: got equals: 10
]

{ #category : #'tests-callbacks' }
TKTFutureTests >> testFutureExecutesInNewProcessRunner [
	| runner future currentRunner |
	runner := TKTNewProcessTaskRunner new.
	future := runner future: [ currentRunner := TKTConfiguration runner ].
	future waitForCompletion: 1 second.
	self assert: currentRunner equals: runner
]

{ #category : #'tests-callbacks' }
TKTFutureTests >> testFutureExecutesInSameLocalProcessRunner [

	| runner future currentRunner |
	runner := TKTLocalProcessTaskRunner new.
	future := runner future: [ currentRunner := TKTConfiguration runner].	
	future waitForCompletion: 1 second.
	
	self assert: currentRunner equals: runner
]

{ #category : #'tests-callbacks' }
TKTFutureTests >> testFutureFailureCallbackExecutesInSameLocalProcessRunner [
	| runner future currentRunner |
	runner := TKTLocalProcessTaskRunner new.
	future := runner future: [ Error signal ].
	future onFailureDo: [ :r | currentRunner := TKTConfiguration runner ].
	future waitForCompletion: 1 second.
	50 milliSecond wait.
	self assert: currentRunner equals: runner
]

{ #category : #'tests-callbacks' }
TKTFutureTests >> testFutureFailureCallbackExecutesInSameNewProcessRunner [
	| runner future currentRunner |
	runner := TKTNewProcessTaskRunner new.
	future := runner future: [ Error signal ].
	future onFailureDo: [ :r | currentRunner := TKTConfiguration runner ].
	future waitForCompletion: 1 second.
	50 milliSecond wait.
	self assert: currentRunner equals: runner
]

{ #category : #'tests-callbacks' }
TKTFutureTests >> testFutureFailureCallbackExecutesInSameWorker [
	| runner future currentRunner |
	runner := TKTWorker new start.
	future := runner future: [ Error signal ].
	future onFailureDo: [ :r | currentRunner := TKTConfiguration runner ].
	future waitForCompletion: 1 second.
	100 milliSeconds wait.
	self assert: currentRunner equals: runner
]

{ #category : #'tests-callbacks' }
TKTFutureTests >> testFutureFailureCallbackIsCalledAfterFailureIsDeployed [

	| future got deployedError |
	future := TKTFuture new.	
	deployedError := Error new.
	future deployFailure: deployedError.
	
	future onFailureDo: [ :error | got := error ].
	50 milliSeconds wait.
	
	self assert: got equals: deployedError
]

{ #category : #'tests-callbacks' }
TKTFutureTests >> testFutureFailureCallbackIsCalledIfFailureWasAlreadyDeployed [

	| future got deployedError |
	future := TKTFuture new.
	future onFailureDo: [ :error | got := error ].
	
	deployedError := Error new.
	future deployFailure: deployedError.
	50 milliSecond wait.
	
	self assert: got equals: deployedError
]

{ #category : #'tests-combinators' }
TKTFutureTests >> testFutureFallbackToFailsIfBothFail [
	| future got fallbackFuture error |
	future := TKTFuture new.
	fallbackFuture := TKTFuture new.
	(future fallbackTo: fallbackFuture) onFailureDo: [ :v | got := v ].
	error := Error new.
	[ error signal ]
		on: Error
		do: [ :e | fallbackFuture deployFailure: e ].
	future deployFailure: Error new.
	50 milliSecond wait.
	self assert: got equals: error
]

{ #category : #'tests-combinators' }
TKTFutureTests >> testFutureFallbackToHasFirstValueIfFirstSuccess [

	| future got fallbackFuture |
	future := TKTFuture new.
	fallbackFuture := TKTFuture new.	

	(future fallbackTo: fallbackFuture) onSuccessDo: [ :v | got := v ].
	
	future deploySuccess: 2.
	50 milliSecond wait.

	self assert: got equals: 2
]

{ #category : #'tests-combinators' }
TKTFutureTests >> testFutureFallbackToHasSecondValueIfFirstFailsAndSecondSuccess [

	| future got fallbackFuture |
	future := TKTFuture new.
	fallbackFuture := TKTFuture new.	

	(future fallbackTo: fallbackFuture) onSuccessDo: [ :v | got := v ].

	fallbackFuture deploySuccess: 2.
	future	 deployFailure: Error new.
	50 milliSecond wait.

	self assert: got equals: 2
]

{ #category : #'tests-combinators' }
TKTFutureTests >> testFutureFirstCompleteOfFailsIfFirstFailsFaster [

	| future got secondFuture error |
	future := TKTFuture new.
	secondFuture := TKTFuture new.
	
	(future firstCompleteOf: secondFuture) onFailureDo: [ :v | got := v ].

	error := Error new.
	[ error signal ]
		on: Error
		do: [ :e | future deployFailure: e ].
	secondFuture deploySuccess: 1.
	50 milliSecond wait.

	self assert: got equals: error
]

{ #category : #'tests-combinators' }
TKTFutureTests >> testFutureFirstCompleteOfFailsIfSecondFailsFaster [

	| future got secondFuture error |
	future := TKTFuture new.
	secondFuture := TKTFuture new.
	
	(future firstCompleteOf: secondFuture) onFailureDo: [ :v | got := v ].

	error := Error new.
	[ error signal ]
		on: Error
		do: [ :e | secondFuture deployFailure: e ].
	future deploySuccess: 1.
	50 milliSecond wait.

	self assert: got equals: error
]

{ #category : #'tests-combinators' }
TKTFutureTests >> testFutureFirstCompleteOfSuccessIfFirstSuccessFaster [
	| future got secondFuture error |
	future := TKTFuture new.
	secondFuture := TKTFuture new.
	(future firstCompleteOf: secondFuture) onSuccessDo: [ :v | got := v ].
	future deploySuccess: 5.
	error := Error new.
	[ error signal ]
		on: Error
		do: [ :e | secondFuture deployFailure: e ].
	50 milliSecond wait.
	self assert: got equals: 5
]

{ #category : #'tests-combinators' }
TKTFutureTests >> testFutureFirstCompleteOfSuccessIfSecondSuccessFaster [

	| future got secondFuture |
	future := TKTFuture new.
	secondFuture := TKTFuture new.
	
	(future firstCompleteOf: secondFuture) onSuccessDo: [ :v | got := v ].

	secondFuture deploySuccess: 5.
	future deployFailure: Error new.
	50 milliSecond wait.
	
	self assert: got equals: 5
]

{ #category : #'tests-combinators' }
TKTFutureTests >> testFutureFlatCollectFailsOnFailingMapFunction [

	| future got error |
	future := TKTFuture new.	
	error := Error new.
	(future flatCollect: [ :v | error signal ]) onFailureDo: [ :v | got := v ].
	
	future deploySuccess: 2.
	50 milliSecond wait.

	self assert: got equals: error
]

{ #category : #'tests-combinators' }
TKTFutureTests >> testFutureFlatCollectFailsOnMappedFutureFailure [
	| future got error innerFuture |
	future := TKTFuture new.
	innerFuture := TKTFuture new.
	(future flatCollect: [ :v | innerFuture ]) onFailureDo: [ :v | got := v ].
	future deploySuccess: 2.
	error := Error new.
	[ error signal ]
		on: Error
		do: [ :e | innerFuture deployFailure: e ].
	50 milliSecond wait.
	self assert: got equals: error
]

{ #category : #'tests-combinators' }
TKTFutureTests >> testFutureFlatCollectFailsOnSelfFailure [
	| future got error innerFuture |
	future := TKTFuture new.
	innerFuture := TKTFuture new.
	(future flatCollect: [ :v | innerFuture ]) onFailureDo: [ :v | got := v ].
	error := Error new.
	[ error signal ]
		on: Error
		do: [ :e | future deployFailure: e ].
	50 milliSecond wait.
	self assert: got equals: error
]

{ #category : #'tests-combinators' }
TKTFutureTests >> testFutureFlatCollectSuccessOnMappedFutureSuccess [

	| future got innerFuture |
	future := TKTFuture new.	
	innerFuture := TKTFuture new.
	(future flatCollect: [ :v | innerFuture ]) onSuccessDo: [ :v | got := v ].
	
	future deploySuccess: 2.
	innerFuture deploySuccess: 42.
	50 milliSecond wait.
	
	self assert: got equals: 42
]

{ #category : #'tests-termination' }
TKTFutureTests >> testFutureIsFinishedIfFailureWasAlreadyDeployed [
	| future error |
	
	future := TKTFuture new.
	error := Error new.
	[ error signal ]
		on: Error
		do: [ :e | future deployFailure: e ].
	self assert: future isFinished
]

{ #category : #'tests-termination' }
TKTFutureTests >> testFutureIsFinishedIfValueWasAlreadyDeployed [

	| future |
	future := TKTFuture new.
	future deploySuccess: 2.
		
	self assert: future isFinished
]

{ #category : #'tests-combinators' }
TKTFutureTests >> testFutureOnDoFailsOnFailingExceptionHandler [
	| future got error error2 |
	future := TKTFuture new.
	error := Error new.
	(future
		on: Error
		do: [ :v | error signal ]) onFailureDo: [ :v | got := v ].
	error2 := Error new.
	[ error2 signal ]
		on: Error
		do: [ :e | future deployFailure: e ].
	50 milliSecond wait.
	self assert: got equals: error
]

{ #category : #'tests-combinators' }
TKTFutureTests >> testFutureOnDoFailsOnTrappingWrongException [
	| future got error |
	future := TKTFuture new.
	(future
		on: NotFound
		do: [ :v | 5 ]) onFailureDo: [ :v | got := v ].
	error := Error new.
	[ error signal ]
		on: Error
		do: [ :e | future deployFailure: e ].
	50 milliSecond wait.
	self assert: got equals: error
]

{ #category : #'tests-combinators' }
TKTFutureTests >> testFutureOnDoSucceedsOnSuccess [

	| future got |
	future := TKTFuture new.	
	(future on: Error do: [ :v | 5 ]) onSuccessDo: [ :v | got := v ].

	future deploySuccess: 2.
	50 milliSecond wait.
	
	self assert: got equals: 2
]

{ #category : #'tests-combinators' }
TKTFutureTests >> testFutureOnDoSucceedsOnTrappingCorrectException [
	| future got error |
	future := TKTFuture new.
	(future
		on: Error
		do: [ :v | 5 ]) onSuccessDo: [ :v | got := v ].
	error := Error new.
	[ error signal ]
		on: Error
		do: [ :e | future deployFailure: e ].
	50 milliSecond wait.
	self assert: got equals: 5
]

{ #category : #'tests-combinators' }
TKTFutureTests >> testFutureSelectFailsOnFailingPredicate [

	| future got error |
	future := TKTFuture new.	
	error := Error new.

	(future select: [ :v | error signal ]) onFailureDo: [ :v | got := v ].

	future deploySuccess: 2.
	50 milliSecond wait.
	
	self assert: got equals: error
]

{ #category : #'tests-combinators' }
TKTFutureTests >> testFutureSelectFailsOnFailure [
	| future got error |
	future := TKTFuture new.
	(future select: [ :v | v even ]) onFailureDo: [ :v | got := v ].
	error := Error new.
	[ error signal ]
		on: Error
		do: [ :e | future deployFailure: e ].
	50 milliSecond wait.
	self assert: got equals: error
]

{ #category : #'tests-combinators' }
TKTFutureTests >> testFutureSelectFailsOnNonSatisfyingCondition [

	| future got |
	future := TKTFuture new.	
	(future select: [ :v | v even ]) onFailureDo: [ :v | got := v ].
	
	future deploySuccess: 3.
	50 milliSecond wait.
	
	self assert: (got isKindOf: NotFound)
]

{ #category : #'tests-combinators' }
TKTFutureTests >> testFutureSelectSuccessOnSatisfyingCondition [

	| future got |
	future := TKTFuture new.	
	(future select: [ :v | v even ]) onSuccessDo: [ :v | got := v ].
	
	future deploySuccess: 2.
	50 milliSecond wait.
	
	self assert: got equals: 2
]

{ #category : #'tests-callbacks' }
TKTFutureTests >> testFutureSuccessCallbackExecutesInNewProcess [

	| runner future processIds processIdsSet |
	processIds := AtomicSharedQueue new.
	
	runner := TKTNewProcessTaskRunner new.
	future := runner future: [ 1 + 1 ].
	
	30 timesRepeat: [
		future onSuccessDo: [ :r | processIds nextPut: Processor activeProcess identityHash ] ].
	
	1 second wait.
	
	processIdsSet := Set new.
	[processIds isEmpty] whileFalse: [ processIdsSet add: processIds next ].
	
	self assert: processIdsSet size equals: 30.
]

{ #category : #'tests-callbacks' }
TKTFutureTests >> testFutureSuccessCallbackExecutesInSameLocalProcessRunner [
	| runner future currentRunner |
	runner := TKTLocalProcessTaskRunner new.
	future := runner future: [ 1 + 1 ].
	future onSuccessDo: [ :r | currentRunner := TKTConfiguration runner ].
	future waitForCompletion: 1 second.
	self assert: currentRunner equals: runner
]

{ #category : #'tests-callbacks' }
TKTFutureTests >> testFutureSuccessCallbackExecutesInSameNewProcessRunner [
	| runner future currentRunner |
	runner := TKTNewProcessTaskRunner new.
	future := runner future: [ 1 + 1 ].
	future onSuccessDo: [ :r | currentRunner := TKTConfiguration runner ].
	future waitForCompletion: 1 second.
	self assert: currentRunner equals: runner
]

{ #category : #'tests-callbacks' }
TKTFutureTests >> testFutureSuccessCallbackExecutesInSameWorker [
	| runner future currentRunner |
	runner := TKTWorker new start.
	future := runner future: [ 1 + 1 ].
	future onSuccessDo: [ :r | currentRunner := TKTConfiguration runner ].
	future waitForCompletion: 1 second.
	self assert: currentRunner equals: runner
]

{ #category : #'tests-callbacks' }
TKTFutureTests >> testFutureSuccessCallbackExecutesInSameWorkerRunner [
	| runner future currentRunner |
	runner := TKTWorker new start.
	future := runner future: [ 1 + 1 ].
	future onSuccessDo: [ :r | currentRunner := TKTConfiguration runner ].
	future waitForCompletion: 1 second.
	self assert: currentRunner equals: runner
]

{ #category : #'tests-callbacks' }
TKTFutureTests >> testFutureSuccessCallbackIsCalledAfterValueIsDeployed [

	| future got |
	future := TKTFuture new.
	future onSuccessDo: [ :value | got := value ].
	
	future deploySuccess: 2.
	50 milliSecond wait.
	
	self assert: got equals: 2
]

{ #category : #'tests-callbacks' }
TKTFutureTests >> testFutureSuccessCallbackIsCalledIfValueWasAlreadyDeployed [

	| future got |
	future := TKTFuture new.
	future deploySuccess: 2.
		
	future onSuccessDo: [ :value | got := value ].

	50 milliSeconds wait.

	self assert: got equals: 2
]

{ #category : #'tests-callbacks' }
TKTFutureTests >> testFutureSuccessCallbackIsNotCalledIfFailureWasAlreadyDeployed [

	| future got |
	future := TKTFuture new.
	future deployFailure: Error new.
		
	future onSuccessDo: [ :value | got := value ].

	self assert: got equals: nil
]

{ #category : #'tests-combinators' }
TKTFutureTests >> testFutureZipFailsIfFirstFails [

	| future zipFuture got error |
	future := TKTFuture new.
	zipFuture := TKTFuture new.
	
	(future zip: zipFuture) onFailureDo: [ :tuple | got := tuple ].

	error := Error new.
	[ error signal ]
		on: Error
		do: [ :e | future deployFailure: e ].
	zipFuture deploySuccess: 5.
	50 milliSecond wait.
	
	self assert: got equals: error
]

{ #category : #'tests-combinators' }
TKTFutureTests >> testFutureZipFailsIfSecondFails [

	| future zipFuture got error |
	future := TKTFuture new.
	zipFuture := TKTFuture new.
	
	(future zip: zipFuture) onFailureDo: [ :tuple | got := tuple ].

	error := Error new.
	[ error signal ]
		on: Error
		do: [ :e | zipFuture deployFailure: e ].

	future deploySuccess: 5.
	50 milliSecond wait.
	
	self assert: got equals: error
]

{ #category : #'tests-combinators' }
TKTFutureTests >> testFutureZipSuccessIfBothSuccess [

	| future zipFuture got |
	future := TKTFuture new.
	zipFuture := TKTFuture new.
	
	(future zip: zipFuture) onSuccessDo: [ :tuple | got := tuple ].

	future deploySuccess: 2.
	zipFuture deploySuccess: 5.
	50 milliSecond wait.
	
	self assert: got equals: #(2 5)
]

{ #category : #'tests-termination' }
TKTFutureTests >> testNewFutureIsNotFinished [

	| future |
	future := TKTFuture new.	
	self assert: future isFinished not
]
