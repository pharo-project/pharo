"
I am an abstract TestCase superclass that tests for correct operation of #hash and #=.

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

{ #category : #tests }
HashAndEqualsTestCase class >> isAbstract [

	^self == HashAndEqualsTestCase
]

{ #category : #running }
HashAndEqualsTestCase >> setUp [
	"Subclasses will add their prototypes into this collection"

	super setUp.
	prototypes := OrderedCollection new
]

{ #category : #tests }
HashAndEqualsTestCase >> testEquality [
	"Check that prototypes report equality correctly"

	prototypes do: [:p | self assert: (EqualityTester with: p) result ]
]

{ #category : #tests }
HashAndEqualsTestCase >> testHash [
	"Check prototypes with equality method hash correctly"

	prototypes do: [:p | self assert: (HashTester with: p) result ]
]
