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

{ #category : 'tests' }
ReDuplicateClassRefactoringTest >> testDuplicateComposedTraitPreservesBothMethodSides [

	| duplicate refactoring source |
	source := TReComposedTraitForDuplication.
	refactoring := ReDuplicateClassRefactoring
		model: model
		copyClass: source
		withName: #TReDuplicatedComposedTrait
		in: #'Refactoring-Transformations-Tests'.
	refactoring generateChanges.
	duplicate := model classNamed: #TReDuplicatedComposedTrait.
	self assertCollection: duplicate traitComposition traits
		hasSameElements: { TReTraitToBeUsedInDuplication }.
	self assert: (duplicate directlyDefinesMethod: #instanceSideValue).
	self assert: (duplicate classSide directlyDefinesMethod: #classSideValue).
	self assert: (duplicate parseTreeForSelector: #instanceSideValue)
		equals: ((model classFor: source) parseTreeForSelector: #instanceSideValue).
	self performAndUndoChange: refactoring changes do: [
		| installed |
		installed := Smalltalk globals at: #TReDuplicatedComposedTrait.
		self assert: installed isTrait.
		self assertCollection: installed traits
			hasSameElements: { TReTraitToBeUsedInDuplication }.
		self assert: (installed includesSelector: #methodInTrait).
		self assert: (installed includesSelector: #instanceSideValue).
		self assert: installed classSideValue equals: 42 ].
	self deny: (Smalltalk globals includesKey: #TReDuplicatedComposedTrait)
]
