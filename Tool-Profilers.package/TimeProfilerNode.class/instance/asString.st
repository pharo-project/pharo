printing
asString
	^ String streamContents: [:s | self item printOn: s total: model tally totalTime: model time tallyExact: false] 