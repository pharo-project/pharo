"
This class should trigger the RBLocalMethodsSameThanTraitRule
"
Class {
	#name : #RBSmalllintTestLocalMethodsSameThanTraitObject,
	#superclass : #RBSmalllintTestSuperclassObject,
	#traits : 'TAssertable',
	#classTraits : 'TAssertable classTrait',
	#category : #'Refactoring-Tests-Critics'
}

{ #category : #asserting }
RBSmalllintTestLocalMethodsSameThanTraitObject >> skip [
	"Don't run this test, and don't mark it as failure"
	SkippingTest signal
]

{ #category : #extensions }
RBSmalllintTestLocalMethodsSameThanTraitObject >> skip: aComment [
	"Don't run this test, and don't mark it as failure"
	SkippingTest signal: aComment
]

{ #category : #asserting }
RBSmalllintTestLocalMethodsSameThanTraitObject >> skipUnless: aBooleanOrBlock [
	" If the assumption in aBooleanOrBlock is not true, abandon the running test
	and mark it as passed. "
	aBooleanOrBlock value ifFalse:
			[ SkippingTest signal: 'Assumption in #skipUnless: failed' ]
]
