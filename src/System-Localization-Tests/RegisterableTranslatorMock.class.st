"
I'm a mock object to mock a translator
"
Class {
	#name : #RegisterableTranslatorMock,
	#superclass : #Object,
	#instVars : [
		'translations'
	],
	#category : #System-Localization-Tests
}

{ #category : #initialization }
RegisterableTranslatorMock >> initialize [
	"Initializes the receiver"
	
	super initialize.
	translations := Dictionary new.
	translations at: #'TestCase' put: 'Testfall'
]

{ #category : #translate }
RegisterableTranslatorMock >> translate: aString [
	"Dispatch to a registered translator to translate the given string or (if there is no translator) return the untranslated string."
	^translations at: aString asSymbol ifAbsent: [ aString ] 

]

{ #category : #translate }
RegisterableTranslatorMock >> translate: aString toLocale: localeID [
 
	^self translate: aString
]
