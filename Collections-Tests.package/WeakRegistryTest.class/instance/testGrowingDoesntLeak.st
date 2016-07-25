tests
testGrowingDoesntLeak

	| w finalized block object executor |
	w := WeakRegistry new: 1.
	finalized := false.
	block := [ :v | finalized := v ].
	object := Object new.
	executor := ObjectFinalizer
		receiver: block
		selector: #value:
		argument: true.
	w add: object executor: executor.
	object := nil. "let it go"
	w addAll: (1 to: 1000). "force growing"
	Smalltalk garbageCollect. "finalize it"
	self assert: w size = 1000.
	self assert: finalized