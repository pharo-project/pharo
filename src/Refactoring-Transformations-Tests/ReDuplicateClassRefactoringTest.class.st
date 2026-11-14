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

