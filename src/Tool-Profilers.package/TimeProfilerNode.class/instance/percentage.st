accessing-computed
percentage 
	^ self item tally asFloat / model time * 100.0 roundTo: 0.1.
