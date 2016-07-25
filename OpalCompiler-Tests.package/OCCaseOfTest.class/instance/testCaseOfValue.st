tests
testCaseOfValue
	| temp |
	
	temp := #case1 caseOf: {
		[#case1]->[ 1 ].
 		[#case2]->[ 2 ] }.
	self assert: temp equals: 1.
	
	temp := #case2 caseOf: {
		[#case1]->[ 1 ].
 		[#case2]->[ 2 ] }.
	self assert: temp equals: 2.
	
	self should: [ 
		temp := #case3 caseOf: {
			[#case1]->[ 1 ].
 			[#case2]->[ 2 ] }
		] raise: Error
