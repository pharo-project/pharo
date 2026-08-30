Class {
	#name : 'ReDuplicateClassRefactoringTest',
	#superclass : 'RBAbstractTransformationTest',
	#category : 'Refactoring-Transformations-Tests-Test',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Test'
}

{ #category : 'tests' }
ReDuplicateClassRefactoringTest >> testDuplicateTrait [

	| addition duplicate refactoring |
	refactoring := ReDuplicateClassRefactoring
		model: model
		copyClass: TReTraitToBeUsedInDuplication
		withName: #TReDuplicatedTrait
		in: #'Refactoring-Transformations-Tests'.

	refactoring generateChanges.
	duplicate := model classNamed: #TReDuplicatedTrait.

	self assert: duplicate class equals: RBTrait.
	self assert: duplicate superclass isNil.
	self assert: (duplicate directlyDefinesMethod: #methodInTrait).
	self
		assert: duplicate packageName
		equals: #'Refactoring-Transformations-Tests'.

	addition := refactoring changes changes
		detect: [ :change | change isKindOf: ReAddClassChange ].
	self assert: addition builder isTrait
]
