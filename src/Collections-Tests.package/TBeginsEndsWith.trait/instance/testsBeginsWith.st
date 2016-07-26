tests - begins ends with
testsBeginsWith
	
	self assert: (self nonEmpty beginsWith:(self nonEmpty copyUpTo: self nonEmpty last)).
	self assert: (self nonEmpty beginsWith:(self nonEmpty )).
	self deny: (self nonEmpty beginsWith:(self nonEmpty copyWith:self nonEmpty first)).