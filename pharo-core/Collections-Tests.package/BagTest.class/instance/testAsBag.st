basic tests
testAsBag

	| aBag |
 
	aBag := Bag new.	
	
	self assert: aBag asBag = aBag.