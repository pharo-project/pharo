tests
testISO3LanguageDefinition

	self 
		assert: (ISOLanguageDefinition iso3LanguageDefinition: 'deu') language = 'German';
		assert: (ISOLanguageDefinition iso3LanguageDefinition: 'fra') language = 'French'