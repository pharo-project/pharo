testing
isValid
	^ (self text includesSpan: self span) and: [ self spanPosition between: 0 and: self span size ]