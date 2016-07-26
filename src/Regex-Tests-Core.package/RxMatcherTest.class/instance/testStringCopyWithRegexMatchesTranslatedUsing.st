testing-extensions
testStringCopyWithRegexMatchesTranslatedUsing
	self assert: ('aabbcc' 
		copyWithRegex: 'b+' 
		matchesTranslatedUsing: [ :each | 
			self assert: each = 'bb'.
			'X' ]) = 'aaXcc'