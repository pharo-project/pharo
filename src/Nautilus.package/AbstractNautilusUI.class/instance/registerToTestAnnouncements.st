announcement registration
registerToTestAnnouncements
	| testCaseClass |
	testCaseClass := Smalltalk at: #TestCase ifAbsent:[^ self].
	testCaseClass historyAnnouncer weak
		when: (Smalltalk at: #TestCaseStarted) send: #testCaseStarted: to: self;
		when: (Smalltalk at: #TestSuiteEnded) send: #testRan: to: self.