tests
testPosixNameConversion

	| locale |
	locale := LocaleID posixName: 'en_us'.
	self 
		assert: locale isoLanguage = 'en';
		assert: locale isoCountry = 'us'