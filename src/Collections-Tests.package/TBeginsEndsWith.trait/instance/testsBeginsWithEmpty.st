tests - begins ends with
testsBeginsWithEmpty
	
	self deny: (self nonEmpty beginsWith:(self empty)).
	self deny: (self empty beginsWith:(self nonEmpty )).
