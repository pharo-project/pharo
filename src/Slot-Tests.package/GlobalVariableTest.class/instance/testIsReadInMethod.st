tests  - properties
testIsReadInMethod

	self assert: (Object binding isReadIn: self class >> testSelector).
	self deny: ((self class environment classNamed: #String) binding 
							isReadIn: self class >> testSelector).