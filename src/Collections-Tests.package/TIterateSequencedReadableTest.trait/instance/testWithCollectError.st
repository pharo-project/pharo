tests - iterate on sequenced reable collections
testWithCollectError
	self should: [self nonEmptyMoreThan1Element with: self empty collect:[:a :b | ]] raise: Error.