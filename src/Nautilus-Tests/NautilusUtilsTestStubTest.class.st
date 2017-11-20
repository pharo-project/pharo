"
This is just a stub test that tests a stub class so NautilusUtilsTest can test method-to-test method mapping.
"
Class {
	#name : #NautilusUtilsTestStubTest,
	#superclass : #TestCase,
	#category : #Nautilus-Tests
}

{ #category : #'instance creation' }
NautilusUtilsTestStubTest >> newStub [
	^ self stubClass new
]

{ #category : #accessing }
NautilusUtilsTestStubTest >> stubClass [
	^ NautilusUtilsTestStub
]

{ #category : #tests }
NautilusUtilsTestStubTest >> testOne [
	| stub |

	stub := self newStub.
	self
		assert: stub one isNil;
		assert: (stub one: #one) == stub;
		assert: stub one = #one.
]

{ #category : #tests }
NautilusUtilsTestStubTest >> testOneTwo [
	| stub |
	stub := self stubClass one: #one two: #two.
	self
		assert: stub one = #one;
		assert: stub two = #two;
		assert: (stub one: #three two: #four) == stub;
		assert: stub one = #three;
		assert: stub two = #four;
		assert: (stub oneTwo: #five) == stub;
		assert: stub one = #three;
		assert: stub two = #five
]

{ #category : #tests }
NautilusUtilsTestStubTest >> testTwo [
	| stub |

	stub := self newStub.
	self
		assert: stub two isNil;
		assert: (stub two: #two) == stub;
		assert: stub two = #two.
]
