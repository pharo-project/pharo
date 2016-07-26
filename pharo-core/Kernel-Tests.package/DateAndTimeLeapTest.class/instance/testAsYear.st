tests
testAsYear
	"A year always starts at January 1"
	self 
		assert: aDateAndTime asYear
		equals: ((Year starting: '02-29-2004' asDate) translateTo: 2 hours ).  
	self
		assert: aDateAndTime asYear
		equals: ((Year starting: '01-01-2004' asDate) translateTo: 2 hours)  
