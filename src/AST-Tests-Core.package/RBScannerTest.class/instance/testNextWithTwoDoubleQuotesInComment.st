next
testNextWithTwoDoubleQuotesInComment
	| source scanner token |
	source := '"only the"" opening"'.
	self
		shouldnt: [ token := (scanner := self buildScannerForText: source) next ]
		raise: SyntaxErrorNotification.
	self assert: token comments first = (1 to: source size)