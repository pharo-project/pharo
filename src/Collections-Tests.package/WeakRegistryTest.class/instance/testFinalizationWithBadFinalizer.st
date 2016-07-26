tests
testFinalizationWithBadFinalizer

	" Test that if error occurs during finalization, the finalization process continues to run normally.

		This test tests that if we adding a faulty finalizer to WeakArray's finalization dependants,
		its not causing troubles to the finalization process.	
	"
	
	| mock1 mock2 sema timedOut |
	
	mock1 := MockFinalizerAction new finalizationAction: [ self signalMockException ].
	mock2 := MockFinalizerAction new finalizationAction: [ sema signal ].
	sema := Semaphore new.

	WeakArray 
		addWeakDependent: mock1;
		addWeakDependent: mock2.
	
	errorWasHere := false.
	
	Smalltalk garbageCollect.
	
	timedOut := sema waitTimeoutSeconds: 1.
	
	self assert: (errorWasHere == true).
	self assert: (timedOut == false)
	