tests - begins ends with
testsEndsWith
	
	self assert: (self nonEmpty endsWith: self nonEmpty copyWithoutFirst).
	self assert: (self nonEmpty endsWith: self nonEmpty).
	self deny: (self nonEmpty endsWith: (self nonEmpty copyWith: self nonEmpty first)).