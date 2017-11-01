Class {
	#name : #LocaleChangedTest,
	#superclass : #TestCase,
	#category : 'System-Localization-Tests'
}

{ #category : #'private accessing' }
LocaleChangedTest >> englishLocale [
	^ Locale isoLanguage: 'en'
]

{ #category : #'private accessing' }
LocaleChangedTest >> germanLocale [
	^ Locale isoLanguage: 'de'
]

{ #category : #tests }
LocaleChangedTest >> testInstanceCreation [
	| instance |
	instance := LocaleChanged from: self englishLocale to: self germanLocale.
	
	self 
		assert: instance oldLocale = self englishLocale;
		assert: instance newLocale = self germanLocale 
	
]
