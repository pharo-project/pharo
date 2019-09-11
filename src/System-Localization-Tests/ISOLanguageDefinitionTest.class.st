"
Unit tests on ISO language definitions
"
Class {
	#name : #ISOLanguageDefinitionTest,
	#superclass : #TestCase,
	#category : #'System-Localization-Tests-Utilities'
}

{ #category : #tests }
ISOLanguageDefinitionTest >> testISO2LanguageDefinition [

	self 
		assert: (ISOLanguageDefinition iso2LanguageDefinition: 'de') language equals: 'German';
		assert: (ISOLanguageDefinition iso2LanguageDefinition: 'fr') language equals: 'French'
]

{ #category : #tests }
ISOLanguageDefinitionTest >> testISO3LanguageDefinition [

	self 
		assert: (ISOLanguageDefinition iso3LanguageDefinition: 'deu') language equals: 'German';
		assert: (ISOLanguageDefinition iso3LanguageDefinition: 'fra') language equals: 'French'
]
