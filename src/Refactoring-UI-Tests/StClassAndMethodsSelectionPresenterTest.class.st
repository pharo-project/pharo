"
A StClassAndMethodsSelectionPresenterTest is a test class for testing the behavior of StClassAndMethodsSelectionPresenter
"
Class {
	#name : 'StClassAndMethodsSelectionPresenterTest',
	#superclass : 'StRequestClassPresenterTest',
	#category : 'Refactoring-UI-Tests-UI',
	#package : 'Refactoring-UI-Tests',
	#tag : 'UI'
}

{ #category : 'accessing' }
StClassAndMethodsSelectionPresenterTest >> newPresenter [

	^ StClassAndMethodsSelectionPresenter on: (ReDuplicateClassDriver new
			   className: 'Class';
			   scopes: { (RBClassEnvironment classes: {
							    ReClassToBeDuplicated.
							    ReClassToBeDuplicated superclass }) };
			   yourself)
]
