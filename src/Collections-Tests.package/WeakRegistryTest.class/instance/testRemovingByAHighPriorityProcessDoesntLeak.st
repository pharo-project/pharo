tests
testRemovingByAHighPriorityProcessDoesntLeak

	| w finalized block hash object executor semaphore |
	w := WeakRegistry new: 1.
	finalized := false.
	block := [ :v | finalized := v ].
	object := Object new.
	executor := ObjectFinalizer
		receiver: block
		selector: #value:
		argument: true.
	hash := object hash.
	w add: hash.
	w add: object executor: executor.
	semaphore := Semaphore new.
	[ 
		object := nil. "let it go"
		w remove: hash.
		semaphore signal ] 
			forkAt: WeakArray runningFinalizationProcess priority + 1.
	semaphore wait.
	Smalltalk garbageCollect. "finalize it"
	self assert: w isEmpty.
	self assert: finalized