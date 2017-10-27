tests - creation
testNewProcessWith

	| hasBlockRun block return passedArguments receivedArgument1 receivedArgument2 |
	hasBlockRun := false.
	block := [ :a :b |
		receivedArgument1 := a.
		receivedArgument2 := b.
		hasBlockRun := true ].
	passedArguments := #(1 2).
	return := block newProcessWith: passedArguments.
	
	"Returns immediately"
	self deny: hasBlockRun.
	
	self assert: (return isKindOf: Process).
	
	"Process has not been scheduled"
	self assert: return isSuspended.
	
	return resume.
	Processor yield.
	
	"Each element in the collection argument was passed separately to the block"
	self assert: { receivedArgument1. receivedArgument2 } equals: passedArguments.