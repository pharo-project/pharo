tests
testHasOption
	self assert: (self commandLine hasOption: #option1).
	self assert: (self commandLine hasOption: #option4).
	self assert: (self commandLine hasOption: #option5).
	self assert: (self commandLine hasOption: #o).
	self deny: (self commandLine hasOption: #foo).
	self deny: (self commandLine hasOption: #option5Value).