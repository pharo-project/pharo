accessing-computed
summaryText
	^ String streamContents: [:strm | rootMessageTally 
		ifNotNil: [rootMessageTally time > 0 
			ifTrue: [strm nextPutAll: ' - '; print: rootMessageTally tally; nextPutAll: ' tallies, ', rootMessageTally time printString, ' msec.'; cr.
				rootMessageTally reportGCStatsOn: strm]]]
