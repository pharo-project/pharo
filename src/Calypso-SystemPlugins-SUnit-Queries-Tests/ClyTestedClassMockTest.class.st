Class {
	#name : #ClyTestedClassMockTest,
	#superclass : #TestCase,
	#category : #'Calypso-SystemPlugins-SUnit-Queries-Tests'
}

{ #category : #running }
ClyTestedClassMockTest >> classUnderTest [
	^ClyTestedClassMock
]

{ #category : #running }
ClyTestedClassMockTest >> newTestedObject [
	^self classUnderTest new
]

{ #category : #tests }
ClyTestedClassMockTest >> testExpectedFailure [
	<expectedFailure>
	self assert: false description: 'it is example test with expected failure'
]

{ #category : #tests }
ClyTestedClassMockTest >> testOne [
	| testedObject |
	testedObject := self newTestedObject.
	self
		assert: testedObject one isNil;
		assert: (testedObject one: #one) identicalTo: testedObject;
		assert: testedObject one equals: #one
]

{ #category : #tests }
ClyTestedClassMockTest >> testOneTwo [
	| testedObject |
	testedObject := self newTestedObject one: #one two: #two.
	self
		assert: testedObject one equals: #one;
		assert: testedObject two equals: #two;
		assert: (testedObject one: #three two: #four) identicalTo: testedObject;
		assert: testedObject one equals: #three;
		assert: testedObject two equals: #four;
		assert: (testedObject oneTwo: #five) identicalTo: testedObject;
		assert: testedObject one equals: #three;
		assert: testedObject two equals: #five
]

{ #category : #tests }
ClyTestedClassMockTest >> testTwo [
	| testedObject |
	testedObject := self newTestedObject.
	self
		assert: testedObject two isNil;
		assert: (testedObject two: #two) identicalTo: testedObject;
		assert: testedObject two equals: #two
]
