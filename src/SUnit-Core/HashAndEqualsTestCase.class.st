"
I am a simple TestCase that tests for correct operation of #hash and #=.

Subclasses of me need to fill my prototypes with suitable objects to be tested.
"
Class {
	#name : #HashAndEqualsTestCase,
	#superclass : #TestCase,
	#instVars : [
		'prototypes'
	],
	#category : #'SUnit-Core-Utilities'
}

{ #category : #running }
HashAndEqualsTestCase >> setUp [
	"subclasses will add their prototypes into this collection"
	super setUp.
	prototypes := OrderedCollection new 
]

{ #category : #tests }
HashAndEqualsTestCase >> testEquality [
	"Check that TextFontChanges report equality correctly"
	prototypes
		do: [:p | self
				should: [(EqualityTester with: p) result]] 
]

{ #category : #tests }
HashAndEqualsTestCase >> testHash [
	"test that TextFontChanges hash correctly"
	prototypes do: [:p |
		self should: [(HashTester with: p) result]] 
]
