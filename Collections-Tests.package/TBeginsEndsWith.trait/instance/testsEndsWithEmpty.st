tests - begins ends with
testsEndsWithEmpty
	
	self deny: (self nonEmpty endsWith: self empty).
	self deny: (self empty endsWith: self nonEmpty).
	