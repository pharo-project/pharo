initialize
createNTemps: numTemps
	^ (1 to: numTemps) collect: [ :i | self newTemp ]