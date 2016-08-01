tests
testInitialization

	| margin |
	
	margin := Margin new.
	
	self assert: margin asMargin == margin.
	
	self 
		assert: margin left 		equals: 0;
		assert: margin right 		equals: 0;
		assert: margin top 		equals: 0;
		assert: margin bottom 	equals: 0;
		assert: margin width 		equals: 0;
		assert: margin height 	equals: 0;
		assert: margin isZero 	equals: true. 
	
	