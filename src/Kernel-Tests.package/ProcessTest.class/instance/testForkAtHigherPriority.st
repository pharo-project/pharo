tests
testForkAtHigherPriority

	| hasBlockRun block return |
	hasBlockRun := false.
	block := [ hasBlockRun := true ].
	return := block forkAt: Processor activeProcess priority + 1.
	
	"Preempts current process"
	self assert: hasBlockRun.