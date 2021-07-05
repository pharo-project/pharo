Class {
	#name : #RBInlineTemporaryTransformationTest,
	#superclass : #RBParametrizedTransformationTest,
	#instVars : [
		'interval',
		'selector',
		'rbClass',
		'resultCode'
	],
	#category : #'Refactoring2-Transformations-Tests-Parametrized'
}

{ #category : #tests }
RBInlineTemporaryTransformationTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #interval -> (24 to: 72) . 
						#rbClass -> #RBRefactoryTestDataApp. 
						#selector -> #inlineMethod.
						#resultCode -> 'inlineMethod
										^ self foo;
													inlineMethod;
													bar' };
		yourself
]

{ #category : #accessing }
RBInlineTemporaryTransformationTest >> interval [

	^ interval
]

{ #category : #accessing }
RBInlineTemporaryTransformationTest >> interval: anObject [

	interval := anObject
]

{ #category : #accessing }
RBInlineTemporaryTransformationTest >> rbClass [

	^ rbClass
]

{ #category : #accessing }
RBInlineTemporaryTransformationTest >> rbClass: anObject [

	rbClass := anObject
]

{ #category : #accessing }
RBInlineTemporaryTransformationTest >> resultCode [

	^ resultCode
]

{ #category : #accessing }
RBInlineTemporaryTransformationTest >> resultCode: anObject [

	resultCode := anObject
]

{ #category : #accessing }
RBInlineTemporaryTransformationTest >> selector1 [

	^ selector
]

{ #category : #accessing }
RBInlineTemporaryTransformationTest >> selector: anObject [

	selector := anObject
]

{ #category : #'failure tests' }
RBInlineTemporaryTransformationTest >> testInlineTemporaryBadInterval [
	self
		shouldFail:
			(RBInlineTemporaryTransformation
				inline: (29 to: 100)
				from: #moveDefinition
				in: RBRefactoryTestDataApp) asRefactoring 
]

{ #category : #'failure tests' }
RBInlineTemporaryTransformationTest >> testInlineTemporaryMutlipleAssignment [
	self
		shouldFail:
			(RBInlineTemporaryTransformation
				inline: (60 to: 83)
				from: #moveDefinition
				in: RBRefactoryTestDataApp) asRefactoring
]

{ #category : #'failure tests' }
RBInlineTemporaryTransformationTest >> testInlineTemporaryReadBeforeWritten [
	self
		shouldFail:
			(RBInlineTemporaryTransformation
				inline: (48 to: 56)
				from: #inlineTemporary
				in: RBRefactoryTestDataApp) asRefactoring
]

{ #category : #tests }
RBInlineTemporaryTransformationTest >> testInlineTemporaryRefactoring [
	| refactoring |
	refactoring := RBInlineTemporaryTransformation
		inline: interval
		from: selector
		in: rbClass.
	refactoring asRefactoring transform.

	self assert: ((refactoring model classNamed: rbClass) parseTreeFor: selector) 
		equals: (self parseMethod: resultCode)
]
