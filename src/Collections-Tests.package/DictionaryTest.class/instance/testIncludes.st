tests - testing
testIncludes
	| o1 o2 newDict |
	self assert: (nonEmptyDict includes: self element).
	
	o1 := 2 @ 3.
	o2 := 2 @ 3.
	self deny: (o1 == o2).
	self assert: (o1 = o2).
	newDict := self collectionClass new.
	newDict at: #a put: o1.
	
	self assert: (newDict includes: o2).
