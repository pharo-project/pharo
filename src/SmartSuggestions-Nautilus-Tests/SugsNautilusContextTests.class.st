Class {
	#name : #SugsNautilusContextTests,
	#superclass : #TestCase,
	#category : #'SmartSuggestions-Nautilus-Tests'
}

{ #category : #tests }
SugsNautilusContextTests >> testContextForNautilusItsANautilusContext [
	| nautilus |
	
	nautilus := NautilusUI new.
	self assert: (SugsAbstractContext contextFor: nautilus) class equals: SugsNautilusContext
]
