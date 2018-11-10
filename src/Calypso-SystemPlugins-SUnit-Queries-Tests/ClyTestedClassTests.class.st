Class {
	#name : #ClyTestedClassTests,
	#superclass : #TestCase,
	#category : #'Calypso-SystemPlugins-SUnit-Queries-Tests'
}

{ #category : #running }
ClyTestedClassTests >> classUnderTest [
	^ClyTestedClass
]

{ #category : #running }
ClyTestedClassTests >> newTestedObject [
	^self classUnderTest new
]

{ #category : #tests }
ClyTestedClassTests >> testExpectedFailure [
	<expectedFailure>
	self assert: false description: 'it is example test with expected failure'
]

{ #category : #tests }
ClyTestedClassTests >> testOne [
	| testedObject |

	testedObject := self newTestedObject.
	self
		assert: testedObject one isNil;
		assert: (testedObject one: #one) == testedObject;
		assert: testedObject one = #one.
]

{ #category : #tests }
ClyTestedClassTests >> testOneTwo [
	| testedObject |
	testedObject := self newTestedObject one: #one two: #two.
	self
		assert: testedObject one = #one;
		assert: testedObject two = #two;
		assert: (testedObject one: #three two: #four) == testedObject;
		assert: testedObject one = #three;
		assert: testedObject two = #four;
		assert: (testedObject oneTwo: #five) == testedObject;
		assert: testedObject one = #three;
		assert: testedObject two = #five
]

{ #category : #tests }
ClyTestedClassTests >> testTwo [
	| testedObject |

	testedObject := self newTestedObject.
	self
		assert: testedObject two isNil;
		assert: (testedObject two: #two) == testedObject;
		assert: testedObject two = #two.
]
