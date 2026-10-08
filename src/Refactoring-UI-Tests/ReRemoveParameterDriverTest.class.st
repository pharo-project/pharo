"
I test ReRemoveParameterDriver. I verify that the driver gathers the modified signature, validates it and runs RBRemoveParameterRefactoring.
"
Class {
	#name : 'ReRemoveParameterDriverTest',
	#superclass : 'ReDriverTest',
	#category : 'Refactoring-UI-Tests',
	#package : 'Refactoring-UI-Tests'
}

{ #category : 'tests' }
ReRemoveParameterDriverTest >> testRemoveParameterExpectSuccess [

	| rbclasses model method driver dialog methodName |
	rbclasses := RBClassEnvironment classes: { RBClassDataForRefactoringTest }.
	model := RBNamespace onEnvironment: rbclasses.
	method := (model classObjectFor: RBClassDataForRefactoringTest) methodFor: 'renameThisMethod:' asSymbol.
	driver := ReRemoveParameterDriver new
		          scopes: { rbclasses }
		          model: model
		          removeParameterFromMethodSignature: method
		          in: RBClassDataForRefactoringTest.
	self setUpDriver: driver.
	methodName := RBMethodName selector: #renameThisMethod arguments: #(  ).
	dialog := MockObject new.
	dialog on: #cancelled respond: false.
	dialog on: #methodName respond: methodName.
	driver requestDialog: dialog.
	driver runRefactoring.
	self assert: driver refactoring model changes changes size equals: 4
]
