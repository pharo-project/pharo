tests
testFork

	| hasBlockRun block return checkAssert |
	checkAssert := Semaphore new.
	hasBlockRun := false.
	block := [ hasBlockRun := true. checkAssert signal.].
	return := block fork.
	
	"Returns immediately, forked block not yet run"
	self deny: hasBlockRun.
	
	"Returns a process - Blue book specifies that it returns the block itself"
	self assert: (return isKindOf: Process).
	
	"Forked process has been scheduled"
	checkAssert wait.
	self assert: hasBlockRun 