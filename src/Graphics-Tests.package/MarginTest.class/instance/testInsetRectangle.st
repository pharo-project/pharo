tests
testInsetRectangle

	| rect |
	
	rect := 2 asMargin insetRectangle: (0@0 extent: 10@10).
	
	self assert: rect equals: (2@2 extent: 6@6).
	
	rect := ( 2@1 ) asMargin insetRectangle: (0@0 extent: 10@10).
	
	self assert: rect equals: (2@1 extent: 6@8).
	
	rect := (Margin top: 1 left: 2 bottom: 3 right: 4) insetRectangle: (0@0 extent: 10@10).
	
	self assert: rect equals: (2@1 extent: 4@6)