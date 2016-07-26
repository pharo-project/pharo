instruction decoding
pushClosureTemps: numTemps
	numTemps timesRepeat: [ self push: nil ]