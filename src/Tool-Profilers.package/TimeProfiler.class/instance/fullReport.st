accessing-computed
fullReport
	"Return the full report of the tally "
	^ String streamContents: [:strm |
		rootMessageTally 
			ifNotNil: [ rootMessageTally report: strm ] ]