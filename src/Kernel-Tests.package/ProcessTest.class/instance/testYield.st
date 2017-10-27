tests
testYield

	| lowerHasRun lowerPriority same1HasRun same2HasRun |
	lowerHasRun := same1HasRun := same2HasRun := false.
	lowerPriority := Processor activeProcess priority - 10 min: 10.
	[ lowerHasRun := true ] forkAt: lowerPriority.
	[ same1HasRun := true ] fork.
	[ same2HasRun := true ] fork.

	Processor yield.
	
	"All processes of same priority have been given a chance to execute"
	self assert: same1HasRun.
	self assert: same2HasRun.
	self deny: lowerHasRun.