"
An OmRandomSuffixStrategyTest is a test class for testing the behavior of OmRandomSuffixStrategy
"
Class {
	#name : #OmRandomSuffixStrategyTest,
	#superclass : #OmSessionStoreNameStrategyTest,
	#category : #OmbuTests
}

{ #category : #tests }
OmRandomSuffixStrategyTest >> testIsNotConstant [

	| names |
	names := (1 to: 7) collect: [:each | strategy nextTo: 'existing' in: self directory ].

	"This should be different"
	self assert: names asSet size equals: names size.
]
