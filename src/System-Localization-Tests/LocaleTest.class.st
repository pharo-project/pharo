"
A unit test class for class Locale
"
Class {
	#name : #LocaleTest,
	#superclass : #TestCase,
	#category : #'System-Localization-Tests'
}

{ #category : #tests }
LocaleTest >> testCurrent [

	self assert: Locale current notNil
	
]
