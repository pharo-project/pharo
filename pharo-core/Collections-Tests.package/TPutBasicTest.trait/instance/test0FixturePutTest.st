tests - fixture
test0FixturePutTest
	self aValue.
	self anotherValue.
	
	self anIndex.
	self nonEmpty isDictionary 
		ifFalse:[self assert: (self anIndex >=1 & self anIndex <= self nonEmpty size).].
	
	self empty.
	self assert: self empty isEmpty .
	
	self nonEmpty.
	self deny: self nonEmpty  isEmpty.