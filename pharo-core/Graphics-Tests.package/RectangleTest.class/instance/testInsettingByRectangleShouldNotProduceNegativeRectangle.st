tests
testInsettingByRectangleShouldNotProduceNegativeRectangle

	| rec1 rec2 |
	rec1 := 0@0 extent: 100@200.
	rec2 := rec1 insetBy: (1@2 rectangle: 5@10).
	
	self assert: rec2 left = 1. 
	self assert: rec2 right = 95. 
	self assert: rec2 top = 2. 
	self assert: rec2 bottom = 190. 
	
	
	rec1 := 0@0 extent: 100@200.
	rec2 := rec1 insetBy: (46@2 rectangle: 55@10).
	
	self assert: rec2 left = 46. 
	self assert: rec2 right = 46. 
	self assert: rec2 top = 2. 
	self assert: rec2 bottom = 190. 
	
	rec1 := 0@0 extent: 100@200.
	rec2 := rec1 insetBy: (46@2 rectangle: 55@10).
	
	self assert: rec2 left = 46. 
	self assert: rec2 right = 46. 
	self assert: rec2 top = 2. 
	self assert: rec2 bottom = 190. 
	
	rec1 := 0@0 extent: 100@200.
	rec2 := rec1 insetBy: (1@102 rectangle: 5@105).
	
	self assert: rec2 left = 1. 
	self assert: rec2 right = 95. 
	self assert: rec2 top = 102. 
	self assert: rec2 bottom = 102. 
