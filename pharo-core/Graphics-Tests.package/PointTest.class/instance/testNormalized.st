tests - testing
testNormalized
	| pt |
	pt:= Point x:2 y:2.
	
	self assert: pt normalized =  ((2/(8 sqrt))@(2/(8 sqrt))).