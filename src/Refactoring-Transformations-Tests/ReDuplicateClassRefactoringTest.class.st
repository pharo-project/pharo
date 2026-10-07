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

{ #category : 'tests' }
ReDuplicateClassRefactoringTest >> testDuplicateClassPreservesHierarchyAndBothMethodSides [

	| duplicate refactoring |
	refactoring := ReDuplicateClassRefactoring
		model: model
		copyClass: RBClassToRename
		withName: #RBDuplicatedClass
		in: #'Refactoring-Transformations-Tests'.
	refactoring generateChanges.
	duplicate := model classNamed: #RBDuplicatedClass.
	self deny: duplicate isTrait.
	self assert: duplicate superclass name equals: #Object.
	self assert: (duplicate directlyDefinesMethod: #method1).
	self assert: (duplicate classSide directlyDefinesMethod: #justForTest).
	self assert: (duplicate parseTreeForSelector: #method1)
		equals: ((model classFor: RBClassToRename) parseTreeForSelector: #method1)
]
