"
A LogicalFontManagerTest is a test class for testing the behavior of LogicalFontManager
"
Class {
	#name : #LogicalFontManagerTest,
	#superclass : #TestCase,
	#category : 'FontInfrastructure-Tests'
}

{ #category : #accessing }
LogicalFontManagerTest >> currentProviderOrError [
	| provider |
	provider := FreeTypeFontProvider readSlotNamed: #current.
	self assert: provider notNil.
	^ provider
]

{ #category : #tests }
LogicalFontManagerTest >> testHasCurrentFreeTypeFontProvider [
	"If it fails, LogicalFontManager unload should be called."
	self assert: (LogicalFontManager current identityIncludesFontProvider: self currentProviderOrError)
	
]
