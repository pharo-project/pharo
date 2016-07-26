testing-comparing
testSyntacticEqualsWithComplexCompositions

	self assert: ((self t1 + (self t2 - {#a. #b}) + (self t3 @ {#x -> #y})) 
		syntacticallyEquals: (self t1 + (self t2 - {#a. #b}) + (self t3 @ {#x -> #y}))).
	self deny: ((self t1 + (self t2 - {#a. #b}) + (self t3 @ {#x -> #y})) 
		syntacticallyEquals: (self t1 + (self t2 - {#A. #B}) + (self t3 @ {#X -> #Y}))).
	
	self assert: ((self t1 @ {#x -> #y} @ {#z -> #w} - {#a. #b} - {#c} + self t2) 
		syntacticallyEquals: (self t1 @ {#x -> #y} @ {#z -> #w} - {#a. #b} - {#c} + self t2)).
	self deny: ((self t1 @ {#x -> #y} @ {#z -> #w} - {#a. #b} - {#c} + self t2) 
		syntacticallyEquals: (self t1 @ {#x -> #y} @ {#z -> #w} - {#a. #b} - {#C} + self t2)).
