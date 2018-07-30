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
	TestSkip signal
]
