tests - puting with indexes
testAtAllPut
	| |
	self nonEmpty atAll: self indexArray put: self aValue..
	
	self indexArray do:
		[:i | self assert: (self nonEmpty at: i)=self aValue ].
	