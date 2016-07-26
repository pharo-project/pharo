tests
testRemovingDoesntLeak

	| w finalized block hash object executor |
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
	object := nil. "let it go"
	w remove: hash.
	Smalltalk garbageCollect. "finalize it"
	self assert: w isEmpty.
	self assert: finalized