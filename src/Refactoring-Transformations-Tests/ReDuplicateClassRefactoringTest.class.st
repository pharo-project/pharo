Class {
	#name : 'ReDuplicateClassRefactoringTest',
	#superclass : 'RBAbstractTransformationTest',
	#category : 'Refactoring-Transformations-Tests-Test',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Test'
}

{ #category : 'tests' }
ReDuplicateClassRefactoringTest >> testDuplicateClassDefinedOnlyInModel [

	model defineClass: [ :builder |
		builder
			superclassName: #Object;
			name: #RBModelOnlyClass;
			package: #'Refactoring-Transformations-Tests' ].
	(ReDuplicateClassRefactoring
		model: model
		copyClass: #RBModelOnlyClass
		withName: #RBModelOnlyClassCopy
		in: #'Refactoring-Transformations-Tests') generateChanges.
	self assert: (model classNamed: #RBModelOnlyClassCopy) superclass name equals: #Object
]

{ #category : 'tests' }
ReDuplicateClassRefactoringTest >> testDuplicateClassPreservesHierarchyAndBothMethodSides [

	| duplicate refactoring |
	refactoring := ReDuplicateClassRefactoring
		model: model
		copyClass: MyClassARoot
		withName: #RBDuplicatedClass
		in: #'Refactoring-Transformations-Tests'.
	refactoring generateChanges.
	duplicate := model classNamed: #RBDuplicatedClass.
	self deny: duplicate isTrait.
	self assert: duplicate superclass name equals: #Object.
	self assert: (duplicate directlyDefinesMethod: #accessing).
	self assert: (duplicate classSide directlyDefinesMethod: #accessingFromClassSide).
	self assert: (duplicate parseTreeForSelector: #accessing)
		equals: ((model classFor: MyClassARoot) parseTreeForSelector: #accessing)
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
		equals: ((model classFor: source) parseTreeForSelector: #instanceSideValue)
]

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
