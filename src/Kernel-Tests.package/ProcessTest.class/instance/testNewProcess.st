tests - creation
testNewProcess

	| hasBlockRun block return |
	hasBlockRun := false.
	block := [ hasBlockRun := true ].
	return := block newProcess.
	
	"Returns immediately"
	self deny: hasBlockRun.
	
	"Returns a process - Blue book specifies the block"
	self assert: (return isKindOf: Process).
	
	"Forked process has not been scheduled"
	self assert: return isSuspended.