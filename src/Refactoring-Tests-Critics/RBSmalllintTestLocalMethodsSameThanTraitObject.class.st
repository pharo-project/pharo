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
RBSmalllintTestLocalMethodsSameThanTraitObject >> assert: aBoolean [
	"We check for true explicitly to make the assertion fail for non booleans"
	aBoolean == true ifFalse: [self signalFailure: 'Assertion failed']
]
