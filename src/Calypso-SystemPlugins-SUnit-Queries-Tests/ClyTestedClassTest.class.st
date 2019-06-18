Class {
	#name : #ClyTestedClassTest,
	#superclass : #TestCase,
	#category : #'Calypso-SystemPlugins-SUnit-Queries-Tests'
}

{ #category : #running }
ClyTestedClassTest >> classUnderTest [
	^ClyTestedClass
]

{ #category : #running }
ClyTestedClassTest >> newTestedObject [
	^self classUnderTest new
]

{ #category : #tests }
ClyTestedClassTest >> testExpectedFailure [
	<expectedFailure>
	self assert: false description: 'it is example test with expected failure'
]

{ #category : #tests }
ClyTestedClassTest >> testOne [
	| testedObject |

	testedObject := self newTestedObject.
	self
		assert: testedObject one isNil;
		assert: (testedObject one: #one) == testedObject;
		assert: testedObject one = #one.
]

{ #category : #tests }
ClyTestedClassTest >> testOneTwo [
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
ClyTestedClassTest >> testTwo [
	| testedObject |

	testedObject := self newTestedObject.
	self
		assert: testedObject two isNil;
		assert: (testedObject two: #two) == testedObject;
		assert: testedObject two = #two.
]
