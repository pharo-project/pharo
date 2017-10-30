tests
testPadLeftTo
	self assert: ('a' padLeftTo: 3) equals: '  a'. 
	self assert: ('a' padLeftTo: 1) equals: 'a'. 
	self assert: ('a' padLeftTo: 0) equals: 'a'.