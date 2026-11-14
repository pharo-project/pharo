Class {
	#name : 'RBInsertNewTraitRefactoringTest',
	#superclass : 'RBAbstractTransformationTest',
	#category : 'Refactoring-Transformations-Tests-Test',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Test'
}

{ #category : 'running' }
RBInsertNewTraitRefactoringTest >> newRefactoring [

	^ (RBInsertNewTraitRefactoring model: model className: #TInsertedTrait)
		  packageName: #'Refactoring-Transformations-Tests';
		  yourself
]

{ #category : 'tests' }
RBInsertNewTraitRefactoringTest >> testInsertTraitDefinesTraitWithMetadata [

	| trait |
	self newRefactoring
		tagName: 'Model';
		comment: 'An inserted trait';
		generateChanges.

	trait := model classNamed: #TInsertedTrait.
	self assert: trait isTrait.
	self assert: trait superclass isNil.
	self assert: trait packageName equals: #'Refactoring-Transformations-Tests'.
	self assert: trait tagName equals: 'Model'.
	self assert: trait comment equals: 'An inserted trait'
]

{ #category : 'tests' }
RBInsertNewTraitRefactoringTest >> testInsertTraitWithCompositionUsesIt [

	| trait |
	self newRefactoring
		traitComposition: TReTraitToBeUsedInDuplication;
		generateChanges.

	trait := model classNamed: #TInsertedTrait.
	self assert: trait isTrait.
	self
		assertCollection: trait traitComposition traits
		hasSameElements: { TReTraitToBeUsedInDuplication }
]

{ #category : 'tests' }
RBInsertNewTraitRefactoringTest >> testInsertTraitWithoutCompositionUsesNoTraits [

	self newRefactoring generateChanges.

	self assert: (model classNamed: #TInsertedTrait) traitComposition traits isEmpty
]
