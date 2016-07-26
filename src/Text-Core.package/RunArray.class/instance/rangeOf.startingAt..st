adding
rangeOf: attr startingAt: startPos
	"Answer an interval that gives the range of attr at index position startPos. An empty interval with start value startPos is returned when the attribute attr is not present at position startPos. self size > 0 is assumed, it is the responsibility of the caller to test for emptiness of self.
Note that an attribute may span several adjancent runs. "

	self at: startPos 
		setRunOffsetAndValue: 
			[:run :offset :value | 
			^(value includes: attr)
				ifFalse: [startPos to: startPos - 1]
				ifTrue:
					[ | firstRelevantPosition lastRelevantPosition idxOfCandidateRun |
					lastRelevantPosition := startPos - offset + (runs at: run) - 1.
					firstRelevantPosition := startPos - offset.
					idxOfCandidateRun := run + 1.
					[idxOfCandidateRun <= runs size 
							 and: [(values at: idxOfCandidateRun) includes: attr]]
						whileTrue:
							[lastRelevantPosition := lastRelevantPosition + (runs at: idxOfCandidateRun).
							idxOfCandidateRun := idxOfCandidateRun + 1]. 
					idxOfCandidateRun := run - 1.
					[idxOfCandidateRun >= 1 
							 and: [(values at: idxOfCandidateRun) includes: attr]]
						whileTrue:
							[firstRelevantPosition := firstRelevantPosition - (runs at: idxOfCandidateRun).
							idxOfCandidateRun := idxOfCandidateRun - 1]. 
 
					firstRelevantPosition to: lastRelevantPosition]]