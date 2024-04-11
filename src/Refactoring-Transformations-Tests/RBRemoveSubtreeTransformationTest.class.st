Class {
	#name : 'RBRemoveSubtreeTransformationTest',
	#superclass : 'RBAbstractTransformationTest',
	#category : 'Refactoring-Transformations-Tests-Test',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Test'
}

{ #category : 'tests' }
RBRemoveSubtreeTransformationTest >> testEmptyCode [

	self shouldFail: (RBRemoveSubtreeTransformation
			 remove: ''
			 fromMethod: #one
			 inClass: self changeMockClass name).

	self shouldFail: (RBRemoveSubtreeTransformation
			 remove: ''
			 fromMethod: #selector:from:
			 inClass: #RBRemoveMethodTransformation)
]

{ #category : 'tests' }
RBRemoveSubtreeTransformationTest >> testFailureExtract [

	self shouldFail: (RBRemoveSubtreeTransformation
			 remove: ':= anInterval'
			 fromMethod: #one
			 inClass: self changeMockClass name).

	self shouldFail: (RBRemoveSubtreeTransformation
			 remove: ':= aSelector'
			 fromMethod: #selector:from:
			 inClass: #RBRemoveMethodTransformation)
]

{ #category : 'tests' }
RBRemoveSubtreeTransformationTest >> testMethodDoesNotExist [

	self shouldFail: (RBRemoveSubtreeTransformation
			 remove: 'selector := aSelector'
			 fromMethod: #two
			 inClass: self changeMockClass name).

	self shouldFail: (RBRemoveSubtreeTransformation
			 remove: 'selector := aSelector'
			 fromMethod: #selector:for:
			 inClass: #RBRemoveMethodTransformation)
]

{ #category : 'tests' }
RBRemoveSubtreeTransformationTest >> testRefactoring [

	| transformation class |
	transformation := (RBRemoveSubtreeTransformation
		                   remove: 'selector := aSelector'
		                   fromMethod: #selector:from:
		                   inClass: #RBRemoveMethodTransformation)
		                  generateChanges.

	self assert: transformation model changes changes size equals: 1.

	class := transformation model classNamed:
		         #RBRemoveMethodTransformation.
	self assert: (class directlyDefinesMethod: #selector:from:).
	self
		assert:
		(class parseTreeForSelector: #selector:from:) body statements size
		equals: 1
]

{ #category : 'tests' }
RBRemoveSubtreeTransformationTest >> testTransform [

	| transformation class |
	transformation := (RBRemoveSubtreeTransformation
		                   remove: '^ 1'
		                   fromMethod: #one
		                   inClass: self changeMockClass name) generateChanges.

	self assert: transformation model changes changes size equals: 1.

	class := transformation model classNamed:
		         self changeMockClass name.
	self assert: (class directlyDefinesMethod: #one).
	self assertEmpty: (class parseTreeForSelector: #one) body statements
]

{ #category : 'tests' }
RBRemoveSubtreeTransformationTest >> testTransformNotSequenceNode [

	| transformation class |
	transformation := RBCompositeTransformation new transformations:
		                  (OrderedCollection
			                   with: (RBAddMethodTransformation
					                    sourceCode: 'printString1 super printString'
					                    in: self changeMockClass name
					                    withProtocol: #accessing )
			                   with: (RBRemoveSubtreeTransformation
					                    remove: 'super printString'
					                    fromMethod: #printString1
					                    inClass: self changeMockClass name)).
	transformation generateChanges.

	self assert: transformation model changes changes size equals: 2.

	class := transformation model classNamed:
		         self changeMockClass name.
	self assert: (class directlyDefinesMethod: #printString1).
	self assertEmpty:
		(class parseTreeForSelector: #printString1) body statements
]
