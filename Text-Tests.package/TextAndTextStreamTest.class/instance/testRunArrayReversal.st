tests
testRunArrayReversal

  	"this tests the reversal of a  RunArray "
	| runArray |
	runArray := RunArray new.
	runArray 
		add: TextEmphasis normal withOccurrences: 5;
		add: TextEmphasis bold withOccurrences: 5;
		add: TextEmphasis normal withOccurrences: 5.
	self assert: (runArray reversed runs size = 3). 