tests
testRunArrayAdjacentMerge

	"this demonstrates that adjancent runs with equal attributes are merged. "
	| runArray |
	runArray := RunArray new.
	runArray 
		add: TextEmphasis normal withOccurrences: 5;
		add: TextEmphasis bold withOccurrences: 5;
		add: TextEmphasis bold withOccurrences: 5.
	self assert: (runArray runs size = 2). 