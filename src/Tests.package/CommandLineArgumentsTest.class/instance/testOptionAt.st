tests
testOptionAt
	self should: [ self commandLine optionAt: #option1 ] raise: Error.
	self should: [ self commandLine optionAt: #option4 ] raise: Error.
	
	self assert: (self commandLine optionAt: #option5) equals: 'option5Value'.
	self should: [ self commandLine optionAt: #foo ] raise: Error.
	
	self assert: (self commandLine optionAt: #o) equals: 'oValue'.
	self should: [ self commandLine optionAt: #x ] raise: Error.