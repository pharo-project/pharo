tests
testFromISOString

	| locale |
	locale := LocaleID isoString: 'en-us'.
	self 
		assert: locale isoLanguage = 'en';
		assert: locale isoCountry = 'us'