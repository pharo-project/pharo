tests
testFromPoint

	| margin |

	margin := Margin fromPoint: 2@1.
	
	self 
		assert: margin left 		equals: 2;
		assert: margin right 		equals: 2;
		assert: margin top 		equals: 1;
		assert: margin bottom 	equals: 1;
		assert: margin width 		equals: 4;
		assert: margin height 	equals: 2.
	
	