tests
testExtendRectangle

	| rect |
	
	rect := 2 asMargin extendRectangle: (0@0 extent: 10@10).
	
	self assert: rect equals: (0@0 extent: 14@14).
	
	rect := ( 2@1 ) asMargin extendRectangle: (0@0 extent: 10@10).
	
	self assert: rect equals: (0@0 extent: 14@12).
	
	rect := (Margin top: 1 left: 2 bottom: 3 right: 4) extendRectangle: (0@0 extent: 10@10).
	
	self assert: rect equals: (0@0 extent: 16@14)