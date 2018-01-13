Class {
	#name : #SugsNautilusContextTests,
	#superclass : #TestCase,
	#category : #'Nautilus-Tests'
}

{ #category : #tests }
SugsNautilusContextTests >> testContextForNautilusItsANautilusContext [
	| nautilus |
	
	nautilus := NautilusUI new.
	self assert: (SugsAbstractContext contextFor: nautilus) class equals: SugsNautilusContext
]
