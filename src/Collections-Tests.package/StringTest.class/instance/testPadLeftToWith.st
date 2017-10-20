tests
testPadLeftToWith
	self assert: ('a' padLeftTo: 3 with: $#) equals: '##a'. 
	self assert: ('a' padLeftTo: 1 with: $#) equals: 'a'. 
	self assert: ('a' padLeftTo: 0 with: $#) equals: 'a'.