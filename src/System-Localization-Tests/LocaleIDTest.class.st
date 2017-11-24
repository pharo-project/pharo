"
A unit test class for class LocaleID
"
Class {
	#name : #LocaleIDTest,
	#superclass : #TestCase,
	#category : #'System-Localization-Tests'
}

{ #category : #'tests - test data' }
LocaleIDTest >> frenchLocaleID [

	^LocaleID isoLanguage: 'fr'
]

{ #category : #'tests - test data' }
LocaleIDTest >> germanLocaleID [

	^LocaleID isoLanguage: 'de'
]

{ #category : #tests }
LocaleIDTest >> testComparision [

	self deny: self germanLocaleID = self frenchLocaleID.
	self assert: self germanLocaleID = self germanLocaleID.	
]

{ #category : #tests }
LocaleIDTest >> testFromISOString [

	| locale |
	locale := LocaleID isoString: 'en-us'.
	self 
		assert: locale isoLanguage = 'en';
		assert: locale isoCountry = 'us'
]

{ #category : #tests }
LocaleIDTest >> testFromSingleISOString [

	| locale |
	locale := LocaleID isoString: 'en'.
	self 
		assert: locale isoLanguage = 'en';
		assert: locale isoCountry isNil
]

{ #category : #tests }
LocaleIDTest >> testHashing [

	self assert: self germanLocaleID hash = self germanLocaleID hash.
	self assert: self frenchLocaleID hash = self frenchLocaleID hash.	

]

{ #category : #tests }
LocaleIDTest >> testInstanceCreationWithISOLanguage [

	|germanLocale|
	germanLocale := LocaleID isoLanguage: 'de'.
	self assert: germanLocale isoLanguage = 'de'
]

{ #category : #tests }
LocaleIDTest >> testInstanceCreationWithISOLanguageAndCountry [

	|locale|
	locale := LocaleID isoLanguage: 'zh' isoCountry: 'CN'.
	self 
		assert: locale isoLanguage = 'zh';
		assert: locale isoCountry = 'CN'
]

{ #category : #tests }
LocaleIDTest >> testPosixNameConversion [

	| locale |
	locale := LocaleID posixName: 'en_us'.
	self 
		assert: locale isoLanguage = 'en';
		assert: locale isoCountry = 'us'
]

{ #category : #tests }
LocaleIDTest >> testPrintString [

	| locale |
	locale := LocaleID isoString: 'en-us'.	
	self assert: locale printString = 'en-us'

]
