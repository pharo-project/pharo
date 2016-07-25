tests
testToString
	| object |
	object := STON listClass withAll: { 1. 0. -1. true. false. nil }.
	self assert: (STON toString: object) = '[1,0,-1,true,false,nil]'