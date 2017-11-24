Class {
	#name : #ISOLanguageDefinitionTest,
	#superclass : #TestCase,
	#category : #'System-Localization-Tests'
}

{ #category : #tests }
ISOLanguageDefinitionTest >> testISO2LanguageDefinition [

	self 
		assert: (ISOLanguageDefinition iso2LanguageDefinition: 'de') language = 'German';
		assert: (ISOLanguageDefinition iso2LanguageDefinition: 'fr') language = 'French'
]

{ #category : #tests }
ISOLanguageDefinitionTest >> testISO3LanguageDefinition [

	self 
		assert: (ISOLanguageDefinition iso3LanguageDefinition: 'deu') language = 'German';
		assert: (ISOLanguageDefinition iso3LanguageDefinition: 'fra') language = 'French'
]
