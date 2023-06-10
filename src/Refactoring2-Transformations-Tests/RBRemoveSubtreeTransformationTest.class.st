Class {
	#name : #RBRemoveSubtreeTransformationTest,
	#superclass : #RBTransformationTest,
	#category : #'Refactoring2-Transformations-Tests-Test'
}

{ #category : #tests }
RBRemoveSubtreeTransformationTest >> testEmptyCode [
	| transformation |
	transformation := (RBRemoveSubtreeTransformation code: '' from: #one in: self changeMock name) transform.
	self assertEmpty: transformation model changes changes.

	self shouldFail: (RBRemoveSubtreeTransformation code: '' from: #selector:from: in: #RBRemoveMethodTransformation) asRefactoring
]

{ #category : #tests }
RBRemoveSubtreeTransformationTest >> testFailureExtract [
	| transformation |
	transformation := (RBRemoveSubtreeTransformation code: ':= anInterval' from: #one in: self changeMock name) transform.
	self assertEmpty: transformation model changes changes.

	self shouldFail: (RBRemoveSubtreeTransformation code: ':= aSelector' from: #selector:from: in: #RBRemoveMethodTransformation) asRefactoring
]

{ #category : #tests }
RBRemoveSubtreeTransformationTest >> testMethodDoesNotExist [
	| transformation |
	transformation := (RBRemoveSubtreeTransformation code: 'selector := aSelector' from: #two in: self changeMock name) transform.
	self assertEmpty: transformation model changes changes.

	self shouldFail: (RBRemoveSubtreeTransformation code: 'selector := aSelector' from: #selector:for: in: #RBRemoveMethodTransformation) asRefactoring
]

{ #category : #tests }
RBRemoveSubtreeTransformationTest >> testRefactoring [

	| transformation class |
	transformation := (RBRemoveSubtreeTransformation
							code: 'selector := aSelector'
							from: #selector:from:
							in: #RBRemoveMethodTransformation)
							asRefactoring transform.

	self assert: transformation model changes changes size equals: 1.

	class := transformation model classNamed: #RBRemoveMethodTransformation.
	self assert: (class directlyDefinesMethod: #selector:from:).
	self assert: (class parseTreeForSelector: #selector:from:) body statements size equals: 1
]

{ #category : #tests }
RBRemoveSubtreeTransformationTest >> testTransform [
	| transformation class |
	transformation := (RBRemoveSubtreeTransformation code: '^ 1' from: #one in: self changeMock name) transform.

	self assert: transformation model changes changes size equals: 1.

	class := transformation model classNamed: self changeMock name asSymbol.
	self assert: (class directlyDefinesMethod: #one).
	self assertEmpty: (class parseTreeForSelector: #one) body statements
]

{ #category : #tests }
RBRemoveSubtreeTransformationTest >> testTransformNotSequenceNode [
	| transformation class |
	transformation := RBCompositeTransformation new
		transformations:
			(OrderedCollection
				with: (RBAddMethodTransformation sourceCode: 'printString1 super printString' in: self changeMock name withProtocols: {#accessing})
				with: (RBRemoveSubtreeTransformation code: 'super printString' from: #printString1 in: self changeMock name)).
	transformation transform.

	self assert: transformation model changes changes size equals: 2.

	class := transformation model classNamed: self changeMock name asSymbol.
	self assert: (class directlyDefinesMethod: #printString1).
	self assertEmpty: (class parseTreeForSelector: #printString1) body statements
]
