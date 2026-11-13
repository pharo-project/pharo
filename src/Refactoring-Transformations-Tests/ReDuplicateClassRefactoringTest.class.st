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
