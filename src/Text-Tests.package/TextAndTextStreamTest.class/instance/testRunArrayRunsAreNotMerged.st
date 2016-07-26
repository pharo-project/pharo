tests
testRunArrayRunsAreNotMerged

	" this demonstrates that different runs are not merged "
	| runArray |
	runArray := RunArray new.
	runArray 
		add: TextEmphasis normal withOccurrences: 5;
		add: TextEmphasis bold withOccurrences: 5;
		add: TextEmphasis normal withOccurrences: 5.
	self assert: (runArray runs size = 3). 