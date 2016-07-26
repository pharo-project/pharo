tests
testGrowingByAHighPriorityProcessDoesntLeak

	| w finalized block object executor semaphore |
	w := WeakRegistry new: 1.
	finalized := false.
	block := [ :v | finalized := v ].
	object := Object new.
	executor := ObjectFinalizer
		receiver: block
		selector: #value:
		argument: true.
	w add: object executor: executor.
	semaphore := Semaphore new.
	[ 
		object := nil. "let it go"
		w addAll: (1 to: 1000). "force growing"
		semaphore signal ] 
			forkAt: WeakArray runningFinalizationProcess priority + 1.
	semaphore wait.
	Smalltalk garbageCollect. "finalize it"
	self assert: w size = 1000.
	self assert: finalized