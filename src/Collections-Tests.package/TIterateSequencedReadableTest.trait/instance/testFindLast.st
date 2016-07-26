tests - iterate on sequenced reable collections
testFindLast

	| element result |
	element := self nonEmptyMoreThan1Element  at:self nonEmptyMoreThan1Element  size.
	 result:=self nonEmptyMoreThan1Element  findLast: [:each | each =element].
	
	self assert: result=self nonEmptyMoreThan1Element  size. 