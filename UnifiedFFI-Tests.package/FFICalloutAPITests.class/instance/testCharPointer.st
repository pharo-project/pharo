tests pointer
testCharPointer
	| result |
	result := self 
		primStr: ('Hello, ' asByteArray copyWith: 0) 
		cat: ('World' asByteArray copyWith: 0).
	self assert: result = 'Hello, World'.