tests
testCaseOfOtherwiseValue
	| temp |
	
	temp := #case1 
		caseOf: {
			[#case1]->[ 1 ].
 			[#case2]->[ 2 ] }
		otherwise: [ 3 ].
	self assert: temp equals: 1.
	
	temp := #case2
		caseOf: {
			[#case1]->[ 1 ].
 			[#case2]->[ 2 ] }
		otherwise: [ 3 ].
	self assert: temp equals: 2.
	
	temp := #case3 
		caseOf: {
			[#case1]->[ 1 ].
 			[#case2]->[ 2 ] }
		otherwise: [ 3 ].
	self assert: temp equals: 3
	
	
