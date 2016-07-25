announcement registration
unregisterToTestAnnouncements
	| testCaseClass |
	testCaseClass := Smalltalk at: #TestCase ifAbsent:[^ self].
	testCaseClass historyAnnouncer unsubscribe: self.
