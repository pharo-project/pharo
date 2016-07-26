tests
testFinalizationWithOnFork

	" Test that if error occurs during finalization, the finalization process continues to run normally.
	
	We addting two executors into weak registry:
	 the faulty one, which causing an error
	 and good one, which signals semaphore
	
	test shows that both finalizers are evaluated and error doesn't prevents a good finalizer from execution
	"
	
	| reg obj1 obj2 sema timedOut |
	
	reg := WeakRegistry new.
	sema := Semaphore new.
	WeakArray removeWeakDependent: reg.
	obj1 := Array new: 1.
	obj2 := Array new: 2.
	
	reg add: obj1 executor: ( MockFinalizerAction new finalizationAction: [ self signalMockException ] ).
	reg add: obj2 executor: ( MockFinalizerAction new finalizationAction: [ sema signal ] ).

	errorWasHere := false.
	
	obj1 := obj2 := nil.
	Smalltalk garbageCollect.
	
	reg finalizeValues.
	
	timedOut := sema waitTimeoutSeconds: 1.
	
	self assert: (errorWasHere == true).
	self assert: (timedOut == false)
	