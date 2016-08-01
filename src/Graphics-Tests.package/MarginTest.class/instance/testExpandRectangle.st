tests
testExpandRectangle

	| rect |
	
	rect := 2 asMargin expandRectangle: (0@0 extent: 10@10).
	
	self assert: rect equals: ((2@2) negated extent: 14@14).
	
	rect := ( 2@1 ) asMargin expandRectangle: (0@0 extent: 10@10).
	
	self assert: rect equals: ((2@1) negated extent: 14@12).
	
	rect := (Margin top: 1 left: 2 bottom: 3 right: 4) expandRectangle: (0@0 extent: 10@10).
	
	self assert: rect equals: ((2@1) negated extent: 16@14)