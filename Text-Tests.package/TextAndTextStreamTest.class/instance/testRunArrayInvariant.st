tests
testRunArrayInvariant

	"this verifies that the fundamental invariant of a RunArray is always satisfied. "
	"see comment below"
	| runArray |
	runArray := RunArray new.
	runArray 
		add: TextEmphasis normal withOccurrences: 5;
		add: TextEmphasis bold withOccurrences: 5;
		add: TextEmphasis normal withOccurrences: 5.
	self assert:
       ((1 to: runArray size) allSatisfy:
           [:idx |  | lastIndex lastOffset lastRun lengthOfPreviousRuns |
               runArray at: idx.  " updates the cached values "
               lastIndex := runArray instVarNamed: 'lastIndex'.
               lastRun := runArray instVarNamed: 'lastRun'.
               lastOffset := runArray instVarNamed: 'lastOffset'.
               lengthOfPreviousRuns 
                   := (1 to: lastRun - 1)
                      inject: 0
                       into: [:sum :idx2 | sum + (runArray runs at: idx2)].
               lastIndex = (lastOffset + lengthOfPreviousRuns + 1) 
           ]
       ). 

" This method is a bit tricky. First, it uses Object>>instVarNamed: to access instance variables for which no accessors are defined. The same method is used by the debuggers and by various inspectors.
The assertion itself explains the meaning of the cached values."