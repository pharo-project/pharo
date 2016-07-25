tests
testVileFinalizer
	" this test checks that manipulation (or accessing) a weak registry 
	does not lead to deadlocks when performed from within a #finalize implementation"
	
	| reg villian proc locked |
	
	reg := WeakRegistry new.
	WeakArray removeWeakDependent: reg.  "to prevent test interference with finalization process"
	
	villian := VileFinalizer new initialize: reg.  "add object with vile finalization to registry"
	
	locked := true.

	proc := [ reg finalizeValues. locked := false ] newProcess.
	villian := nil.	
	Smalltalk garbageCollect.
	
	proc resume.
	100 milliSeconds wait. "give chance for created process to run "
	
	proc isTerminated ifFalse: [ proc terminate ].

	self assert: locked == false.
