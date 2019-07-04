"
A unit test class for class LocaleID
"
Class {
	#name : #LocaleIDTest,
	#superclass : #TestCase,
	#category : #'System-Localization-Tests-Locales'
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

	self deny: self germanLocaleID equals: self frenchLocaleID.
	self assert: self germanLocaleID equals: self germanLocaleID.	
]

{ #category : #tests }
LocaleIDTest >> testFromISOString [

	| locale |
	locale := LocaleID isoString: 'en-us'.
	self 
		assert: locale isoLanguage equals: 'en';
		assert: locale isoCountry equals: 'us'
]

{ #category : #tests }
LocaleIDTest >> testFromSingleISOString [

	| locale |
	locale := LocaleID isoString: 'en'.
	self 
		assert: locale isoLanguage equals: 'en';
		assert: locale isoCountry isNil
]

{ #category : #tests }
LocaleIDTest >> testHashing [

	self assert: self germanLocaleID hash equals: self germanLocaleID hash.
	self assert: self frenchLocaleID hash equals: self frenchLocaleID hash.	

]

{ #category : #tests }
LocaleIDTest >> testInstanceCreationWithISOLanguage [

	|germanLocale|
	germanLocale := LocaleID isoLanguage: 'de'.
	self assert: germanLocale isoLanguage equals: 'de'
]

{ #category : #tests }
LocaleIDTest >> testInstanceCreationWithISOLanguageAndCountry [

	|locale|
	locale := LocaleID isoLanguage: 'zh' isoCountry: 'CN'.
	self 
		assert: locale isoLanguage equals: 'zh';
		assert: locale isoCountry equals: 'CN'
]

{ #category : #tests }
LocaleIDTest >> testPosixNameConversion [

	| locale |
	locale := LocaleID posixName: 'en_us'.
	self 
		assert: locale isoLanguage equals: 'en';
		assert: locale isoCountry equals: 'us'
]

{ #category : #tests }
LocaleIDTest >> testPrintString [

	| locale |
	locale := LocaleID isoString: 'en-us'.	
	self assert: locale printString equals: 'en-us'

]
