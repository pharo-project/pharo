"
Tests for NaturalLanguageTranslator
"
Class {
	#name : #NaturalLanguageTranslatorTest,
	#superclass : #TestCase,
	#instVars : [
		'previousRegistered'
	],
	#category : #'System-Localization-Tests-Translations'
}

{ #category : #running }
NaturalLanguageTranslatorTest >> setUp [

	super setUp.
	previousRegistered := NaturalLanguageTranslator current
]

{ #category : #running }
NaturalLanguageTranslatorTest >> tearDown [

	NaturalLanguageTranslator current: previousRegistered.
	super tearDown
]

{ #category : #tests }
NaturalLanguageTranslatorTest >> testRegisterCustomTranslator [

	NaturalLanguageTranslator current: RegisterableTranslatorMock new.
	
	"Our custom translator translates the string differently"
	self assert: 'TestCase' translated equals: 'Testfall'
	
	
]

{ #category : #tests }
NaturalLanguageTranslatorTest >> testTranslatePureString [

	self assert: 'Pharo' translated equals: 'Pharo'
]
