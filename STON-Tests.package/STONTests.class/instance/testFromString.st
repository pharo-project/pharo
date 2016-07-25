tests
testFromString
	| object |
	object := STON listClass withAll: { 1. 0. -1. true. false. nil }.
	self assert: (STON fromString: '[1,0,-1,true,false,nil]') = object