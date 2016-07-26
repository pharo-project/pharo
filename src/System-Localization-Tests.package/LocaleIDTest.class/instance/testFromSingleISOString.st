tests
testFromSingleISOString

	| locale |
	locale := LocaleID isoString: 'en'.
	self 
		assert: locale isoLanguage = 'en';
		assert: locale isoCountry isNil