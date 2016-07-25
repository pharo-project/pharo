tests
testInstanceCreationWithISOLanguageAndCountry

	|locale|
	locale := LocaleID isoLanguage: 'zh' isoCountry: 'CN'.
	self 
		assert: locale isoLanguage = 'zh';
		assert: locale isoCountry = 'CN'