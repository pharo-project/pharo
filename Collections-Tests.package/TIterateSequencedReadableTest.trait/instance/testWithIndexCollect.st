tests - iterate on sequenced reable collections
testWithIndexCollect
	
	| result index collection |
	index := 0.
	collection := self nonEmptyMoreThan1Element .
	result := collection  withIndexCollect: [:each :i | 
		self assert: i = (index := index + 1).	
		self assert: i = (collection  indexOf: each) .
		each] . 
	
	1 to: result size do:[: i | self assert: (result at:i)= (collection at: i)].
	self assert: result size = collection size.