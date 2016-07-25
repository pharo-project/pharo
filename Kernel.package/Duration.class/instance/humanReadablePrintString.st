printing
humanReadablePrintString
	"Return a String with a human readable representation of me"

	"99999 atRandom seconds humanReadablePrintString"
	"99999999 atRandom milliSeconds humanReadablePrintString"	
	"99999999999999 atRandom nanoSeconds humanReadablePrintString"
	
	^ String streamContents: [ :out | 
		self printHumanReadableOn: out ]