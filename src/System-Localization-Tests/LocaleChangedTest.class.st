"
Unit tests for LocaleChanged announcement
"
Class {
	#name : #LocaleChangedTest,
	#superclass : #TestCase,
	#category : #'System-Localization-Tests-Locales'
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
		assert: instance oldLocale equals: self englishLocale;
		assert: instance newLocale equals: self germanLocale 
	
]
