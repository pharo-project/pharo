tests-valid extensions
testWordCanExtendWord

	self should: [ :builder | 
			builder layoutClass: WordLayout ]
		extendWith: [ :builder | 
			builder layoutClass: WordLayout ]