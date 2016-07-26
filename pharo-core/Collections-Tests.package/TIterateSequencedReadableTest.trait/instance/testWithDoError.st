tests - iterate on sequenced reable collections
testWithDoError
	
	self should: [self nonEmptyMoreThan1Element with: self empty do:[:a :b | ]] raise: Error.