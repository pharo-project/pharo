tests
testCaseOfEffect
	| temp |
	
	#case1 caseOf: {
		[#case1]->[ temp := 1 ].
 		[#case2]->[ temp := 2 ] }.
	self assert: temp equals: 1.
	
	#case2 caseOf: {
		[#case1]->[ temp := 1 ].
 		[#case2]->[ temp := 2 ] }.
	self assert: temp equals: 2.
	
	self should: [ 
		#case3 caseOf: {
			[#case1]->[ 1 ].
 			[#case2]->[ 2 ] }
		] raise: Error