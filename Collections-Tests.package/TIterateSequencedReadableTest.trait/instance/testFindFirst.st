tests - iterate on sequenced reable collections
testFindFirst

	| element result |
	element := self nonEmptyMoreThan1Element   at:1.
	 result:=self nonEmptyMoreThan1Element  findFirst: [:each | each =element].
	
	self assert: result=1. 