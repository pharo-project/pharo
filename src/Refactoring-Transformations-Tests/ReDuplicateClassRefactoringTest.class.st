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
	self assert: (model includesClassNamed: #RBModelOnlyClassCopy)
]


{ #category : 'tests' }
ReDuplicateClassRefactoringTest >> testDuplicateTrait [

	| refactoring duplicate addition |
	refactoring := ReDuplicateClassRefactoring
		model: model
		copyClass: TReTraitToBeUsedInDuplication
		withName: #TReDuplicatedTrait
		in: #'Refactoring-Transformations-Tests'.
	refactoring generateChanges.
	duplicate := model classNamed: #TReDuplicatedTrait.
	self assert: duplicate isTrait.
	self assert: (duplicate directlyDefinesMethod: #methodInTrait).
	addition := refactoring changes changes
		detect: [ :change | change isKindOf: ReAddClassChange ].
	self assert: addition builder isTrait
]
