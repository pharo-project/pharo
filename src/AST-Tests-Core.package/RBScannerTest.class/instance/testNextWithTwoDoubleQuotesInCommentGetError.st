next
testNextWithTwoDoubleQuotesInCommentGetError
	| source scanner token |
	source := '"only the"" opening'.
	self
		should: [ token := (scanner := self buildScannerForText: source) next ]
		raise: SyntaxErrorNotification