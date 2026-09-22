"
A StRenameMethodPreviewPresenterTest is a test class for testing the behavior of StRenameMethodPreviewPresenter
"
Class {
	#name : 'StRenameMethodPreviewPresenterTest',
	#superclass : 'TestCase',
	#category : 'Refactoring-UI-Tests-UI',
	#package : 'Refactoring-UI-Tests',
	#tag : 'UI'
}

{ #category : 'running' }
StRenameMethodPreviewPresenterTest >> setUpDriver [
	
	| rbclasses model method |
	
	rbclasses := RBClassEnvironment classes: { MySubAccessingSuperclassState }.
	model := RBNamespace onEnvironment: rbclasses.
	method := (model classObjectFor: MySubAccessingSuperclassState) methodFor: #accessingSharedVariable.
	^ ReRenameMethodDriver2 new
		          scopes: { rbclasses }
		          model: model
		          renameMethodSignature: method
		          in: MySubAccessingSuperclassState.
]

{ #category : 'tests' }
StRenameMethodPreviewPresenterTest >> testExistingMethodNameDisplaysValidationError [
"Verify that entering an existing method name displays a validation error"

	| driver presenter|
	
	driver := self setUpDriver .
		
	presenter := StRenameMethodPreviewPresenter new.
	presenter 
		driver: driver;
		scopes: driver scopes.
		
	presenter validateName: #accessing.
	
	self assert: presenter validationLabel label 
		  equals: '"accessing" is already defined in this class'
]

{ #category : 'tests' }
StRenameMethodPreviewPresenterTest >> testMethodNameDisplaysBehaviorWarning [
"Verify that a method name that conflicts with an inherited method displays a behavior warning."

	| driver presenter|
	
	driver := self setUpDriver .
		
	presenter := StRenameMethodPreviewPresenter new.
	presenter 
		driver: driver;
		scopes: driver scopes.
		
	presenter defaultLayout .
	presenter validateName: #initialize.
	presenter updateBehaviorPanelVisibility: #initialize.
	
	self assert: presenter behaviorMessageLabel label 
		  equals: 'This name conflicts with an inherited method and may change program behavior.'
]

{ #category : 'tests' }
StRenameMethodPreviewPresenterTest >> testValidMethodNamePassesApplicabilityPreconditions [
"Verify that a valid method name passes the applicability preconditions."

	| driver presenter|
	
	driver := self setUpDriver .
		
	presenter := StRenameMethodPreviewPresenter new.
	presenter 
		driver: driver;
		scopes: driver scopes.
		
	presenter validateName: #newName.
	
	self assert: presenter validationLabel label isEmpty 
]
