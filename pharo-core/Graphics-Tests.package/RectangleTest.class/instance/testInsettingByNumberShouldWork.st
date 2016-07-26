tests
testInsettingByNumberShouldWork

	| rec1 rec2 |
	rec1 := 0@0 extent: 100@200.
	rec2 := rec1 insetBy: 30.
	
	self assert: rec2 height = 140. 
	self assert: rec2 width = 40. 
	
	rec1 := 0@0 extent: 100@200.
	rec2 := rec1 insetBy: 50.
	
	self assert: rec2 height = 100. 
	self assert: rec2 width = 0. 
	