Class {
	#name : #NECPreferencesTest,
	#superclass : #TestCase,
	#category : #'NECompletion-Tests'
}

{ #category : #tests }
NECPreferencesTest >> testInitializeSmartCharactersMapping [
	self
		assert: (NECPreferences smartCharactersMapping at: $()
		equals: ($) -> true)
]

{ #category : #tests }
NECPreferencesTest >> testInitializeSmartInverseMapping [
	self
		assert: (NECPreferences smartInverseMapping at: $))
		equals: $(
]

{ #category : #tests }
NECPreferencesTest >> testSmartCharactersLeftFor [
	"Both the left and right of a pair should return the left character"
	self 
		assert: (NECPreferences smartCharactersLeftFor: $()
		equals: $(.
	self 
		assert: (NECPreferences smartCharactersLeftFor: $))
		equals: $(.
	
	"Otherwise return nil"
	self 
		assert: (NECPreferences smartCharactersLeftFor: $a)
		equals: nil.
]

{ #category : #tests }
NECPreferencesTest >> testSmartCharactersRightFor [
	"Both the left and right of a pair should return the right character"
	self 
		assert: (NECPreferences smartCharactersRightFor: $()
		equals: $).
	self 
		assert: (NECPreferences smartCharactersRightFor: $))
		equals: $).
	
	"Otherwise return nil"
	self 
		assert: (NECPreferences smartCharactersRightFor: $a)
		equals: nil.
]
