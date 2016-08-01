tests
testConstruction

	| margin |
	
	margin := Margin top: 1 left: 2 bottom: 3 right: 4.
	
	self assert: margin asMargin == margin.
	
	self 
		assert: margin left 		equals: 2;
		assert: margin right 		equals: 4;
		assert: margin top 		equals: 1;
		assert: margin bottom 	equals: 3;
		assert: margin width 		equals: 6;
		assert: margin height 	equals: 4.
	
	