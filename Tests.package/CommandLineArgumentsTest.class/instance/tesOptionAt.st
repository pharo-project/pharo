tests
tesOptionAt
	self assert: (self commandLine optionAt: #option1) equals: nil.
	self assert: (self commandLine optionAt: #option4) equals: nil.
	self assert: (self commandLine optionAt: #option5) equals: 'option5Value'.
	self assert: (self commandLine optionAt: #foo) equals: nil.
	self assert: (self commandLine optionAt: #o) equals: 'oValue'.