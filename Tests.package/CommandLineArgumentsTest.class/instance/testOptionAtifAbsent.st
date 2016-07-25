tests
testOptionAtifAbsent
	self assert: (self commandLine optionAt: #option1 ifAbsent: [ nil ]) equals: nil.
	self assert: (self commandLine optionAt: #option4 ifAbsent: [ nil ]) equals: nil.
	self assert: (self commandLine optionAt: #option5) equals: 'option5Value'.
	self assert: (self commandLine optionAt: #foo ifAbsent: [ nil ]) equals: nil.
	self assert: (self commandLine optionAt: #o) equals: 'oValue'.
	self assert: (self commandLine optionAt: #x ifAbsent: [ nil ]) equals: nil.