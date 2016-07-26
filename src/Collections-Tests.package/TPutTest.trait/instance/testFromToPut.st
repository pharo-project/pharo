tests - puting with indexes
testFromToPut
	
	| collection index |
	index := self indexArray anyOne.
	collection := self nonEmpty copy.
	collection from: 1 to: index  put: self aValue..
	1 to: index do:
		[:i | self assert: (collection at: i)= self aValue].
	(index +1) to: collection size do:
		[:i | self assert: (collection at:i)= (self nonEmpty at:i)].