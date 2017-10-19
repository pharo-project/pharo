tests - termination
testTerminateActive

	| lastStatementEvaluated block1HasRun block2HasRun |
	block1HasRun := block2HasRun := lastStatementEvaluated := false.
	[
		block1HasRun := true.
		Processor activeProcess terminate.
		lastStatementEvaluated := true ] fork.
	
	[
		block2HasRun := true.
		Processor terminateActive.
		lastStatementEvaluated := true ] fork.
	
	Processor yield.
	
	"Expressions following terminate are never executed"
	self assert: block1HasRun.
	self assert: block2HasRun.
	self deny: lastStatementEvaluated.