performance
measure: measuredBlock 
	shouldProfile 
		ifTrue: [Smalltalk tools timeProfiler onBlock: [10 timesRepeat: measuredBlock]].
	realTime := measuredBlock timeToRun