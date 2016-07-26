tests
testRunArrayRunsSize

 	"this demonstrates that the size of a run array is the sum of the sizes of its runs. "
	| runArray |
	runArray := RunArray new.
  	runArray 
		add: TextEmphasis normal withOccurrences: 5;
		add: TextEmphasis bold withOccurrences: 5;
		add: TextEmphasis normal withOccurrences: 5.
	self assert: (runArray size = 15). 