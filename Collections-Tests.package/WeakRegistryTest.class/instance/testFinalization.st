tests
testFinalization

	| w finalized block object |
	w := WeakRegistry new: 1.
	finalized := false.
	block := [ :v | finalized := v ].
	object := ObjectFinalizer "an object that responds to #finalize"
		receiver: block
		selector: #value:
		argument: true.
	w add: object.
	object := nil. "let it go"
	Smalltalk garbageCollect. "finalize it"

	"This is an odd issue. It seems that in some situations the finalization
	process doesn't run 'in time' for the isEmpty assertion below to succeed.
	This really *shouldn't* happen since isEmpty is full of real sends and
	there ought to be an interrupt check in there somewhere. However,
	since there are no real-time guarantees about finalization, it's fair to
	just wait a little to ensure that the finalization process has been run."
	(Delay forMilliseconds: 100) wait.

	self assert: w isEmpty.
	self assert: finalized