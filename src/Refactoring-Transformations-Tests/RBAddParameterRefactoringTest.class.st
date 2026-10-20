Class {
	#name : 'RBAddParameterRefactoringTest',
	#superclass : 'RBAbstractRefactoringTest',
	#category : 'Refactoring-Transformations-Tests-Test',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Test'
}

{ #category : 'tests' }
RBAddParameterRefactoringTest >> testWarnWhenExistingSelector [
	"Adding an argument whose resulting selector is already defined in the class must fail."
	| newArg |
	
	newArg := RBArgumentName name: 'anObject' value: 'nil'.
	self shouldWarn: (
		RBAddParameterRefactoring
			model: model
			addParameterToMethod: #called:on:
			in: RBClassDataForRefactoringTest
			newSelector: #textInput:name:symbol:
			permutation: #( 1 2 -1 )
			newArgs:{ newArg } )
]
