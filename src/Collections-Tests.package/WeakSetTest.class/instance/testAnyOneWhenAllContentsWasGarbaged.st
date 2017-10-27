tests
testAnyOneWhenAllContentsWasGarbaged
	| ws o |
	o := Object new.
	ws := WeakSet with: o.
	o := nil.
	Smalltalk garbageCollect.
	self assert: ws anyOne equals: nil