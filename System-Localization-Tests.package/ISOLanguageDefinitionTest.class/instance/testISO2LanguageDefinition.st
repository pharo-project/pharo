tests
testISO2LanguageDefinition

	self 
		assert: (ISOLanguageDefinition iso2LanguageDefinition: 'de') language = 'German';
		assert: (ISOLanguageDefinition iso2LanguageDefinition: 'fr') language = 'French'