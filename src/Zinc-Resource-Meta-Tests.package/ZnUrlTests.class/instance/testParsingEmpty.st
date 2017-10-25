testing
testParsingEmpty
	| url |
	url := ZnUrl fromString: ''.
	self assert: url isEmpty.
	url := ZnUrl fromString: '/'.
	self assert: url isEmpty.
	url := ZnUrl fromString: '/./foo/../'.
	self assert: url isEmpty.
	url := ZnUrl fromString: '//'.
	self deny: url isEmpty