Class {
	#name : 'ReDuplicateClassRefactoringTest',
	#superclass : 'TestCase',
	#category : 'Refactoring-Transformations-Tests-Test',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Test'
}

{ #category : 'tests' }
ReDuplicateClassRefactoringTest >> testEveryChangeIsInScopeOfTheSourceClass [

	| refactoring changes scopes |
	refactoring := ReDuplicateClassRefactoring
		copyClass: Point
		withName: #ReDuplicateClassScopeProbe.
	refactoring
		instanceMethods: #(  );
		classMethods: #(  ).
	changes := refactoring generateChanges changesToDisplayIn: nil.
	scopes := {
		(RBClassEnvironment classes: { Point }).
		(RBClassHierarchyEnvironment class: Point).
		(RBPackageEnvironment packageName: Point package name) }.

	self assert: changes notEmpty.
	scopes do: [ :env |
		self assert: (changes allSatisfy: [ :change | change isInScope: env ]) ]
]
