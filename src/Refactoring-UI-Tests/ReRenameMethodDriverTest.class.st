Class {
	#name : 'ReRenameMethodDriverTest',
	#superclass : 'ReDriverTest',
	#category : 'Refactoring-UI-Tests',
	#package : 'Refactoring-UI-Tests'
}

{ #category : 'tests' }
ReRenameMethodDriverTest >> testInvalidNameFollowedByAValidNameExpectSuccess [

	| rbclasses driver model method dialog |
	rbclasses := RBClassEnvironment classes: { MyClassARoot }.
	model := RBNamespace onEnvironment: rbclasses.
	method := (model classObjectFor: MyClassARoot) methodFor: 'accessingSharedVariable' asSymbol.
	driver := ReRenameMethodDriver new
		          scopes: { rbclasses }
		          model: model
		          renameMethodSignature: method
		          in: MyClassARoot.

	self setUpDriver: driver.
	
	dialog := MockObject new.
	dialog
		on: #methodName
		respond: (RBMethodName selector: '2a' asSymbol arguments: #(  )).
	dialog
		on: #methodName
		respond: (RBMethodName selector: #testName arguments: #(  )).

	driver requestDialog: dialog.

	driver runRefactoring.

	self assert: driver refactoring model changes changes size equals: 4
]

{ #category : 'tests' }
ReRenameMethodDriverTest >> testValidNameExpectSuccess [

	| rbclasses driver model method dialog |
	rbclasses := RBClassEnvironment classes: { MyClassARoot }.
	model := RBNamespace onEnvironment: rbclasses.
	method := (model classObjectFor: MyClassARoot) methodFor: 'accessingSharedVariable' asSymbol.
	driver := ReRenameMethodDriver new
		          scopes: { rbclasses }
		          model: model
		          renameMethodSignature: method
		          in: MyClassARoot.

	self setUpDriver: driver.
	
	dialog := MockObject new.
	dialog
		on: #methodName
		respond: (RBMethodName selector: #newName arguments: #(  )).
	driver requestDialog: dialog.

	driver runRefactoring.

	self assert: driver refactoring model changes changes size equals: 4
]
