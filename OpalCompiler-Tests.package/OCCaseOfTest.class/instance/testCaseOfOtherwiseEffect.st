tests
testCaseOfOtherwiseEffect
	| temp |
	
	#case1 
		caseOf: {
			[#case1]->[ temp := 1 ].
 			[#case2]->[ temp := 2 ] }
		otherwise: [ temp := 3 ].
	self assert: temp equals: 1.
	
	#case2
		caseOf: {
			[#case1]->[ temp := 1 ].
 			[#case2]->[ temp := 2 ] }
		otherwise: [ temp := 3 ].
	self assert: temp equals: 2.
	
	#case3 
		caseOf: {
			[#case1]->[ temp := 1 ].
 			[#case2]->[ temp := 2 ] }
		otherwise: [ temp := 3 ].
	self assert: temp equals: 3
	
	
