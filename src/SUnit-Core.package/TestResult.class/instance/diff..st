diff
diff: aTestResult
	"Return a collection that contains differences"
	| passed1Selectors failed1Selectors errors1Selectors passed2Selectors failed2Selectors errors2Selectors |
	passed1Selectors := self passed collect: [:testCase | testCase selector].
	failed1Selectors := self failures collect: [:testCase | testCase selector].
	errors1Selectors := self errors collect: [:testCase | testCase selector].

	passed2Selectors := aTestResult passed collect: [:testCase | testCase selector].
	failed2Selectors := aTestResult failures collect: [:testCase | testCase selector].
	errors2Selectors := aTestResult errors collect: [:testCase | testCase selector].
	
	^ {passed1Selectors copyWithoutAll: passed2Selectors .
		failed1Selectors copyWithoutAll: failed2Selectors .
		errors1Selectors copyWithoutAll: errors2Selectors}