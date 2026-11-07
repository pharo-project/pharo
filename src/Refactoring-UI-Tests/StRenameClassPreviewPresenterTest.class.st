"
A StRenameClassPreviewPresenterTest is a test class for testing the behavior of StRenameClassPreviewPresenter
"
Class {
	#name : 'StRenameClassPreviewPresenterTest',
	#superclass : 'TestCase',
	#category : 'Refactoring-UI-Tests-UI',
	#package : 'Refactoring-UI-Tests',
	#tag : 'UI'
}

{ #category : 'running' }
StRenameClassPreviewPresenterTest >> newDriver [

	^ ReRenameClassDriver2 new
		oldName: #ReClassToBeRenamed;
		scopes: { RBBrowserEnvironment default };
		configureRefactoring;
		yourself
]

{ #category : 'running' }
StRenameClassPreviewPresenterTest >> newPresenterFor: aDriver [

	^ StRenameClassPreviewPresenter new
		driver: aDriver;
		scopes: { RBBrowserEnvironment default };
		name: 'ReClassToBeRenamed';
		yourself
]

{ #category : 'tests' }
StRenameClassPreviewPresenterTest >> testClearingNameClearsErrorAndDisablesApply [

	| presenter |
	presenter := self newPresenterFor: self newDriver.
	presenter newNameInput text: 'Object'.
	self deny: presenter validationLabel label isEmpty.

	presenter newNameInput text: ''.

	self assert: presenter validationLabel label equals: ''.
	self deny: presenter buttonOk isEnabled.
	self assert: presenter changes isEmptyOrNil
]

{ #category : 'tests' }
StRenameClassPreviewPresenterTest >> testCorrectingInvalidNameClearsErrorAndEnablesApply [

	| presenter |
	presenter := self newPresenterFor: self newDriver.
	presenter newNameInput text: 'Object'.
	self deny: presenter validationLabel label isEmpty.
	self deny: presenter buttonOk isEnabled.

	presenter newNameInput text: 'IrisRenamedClass'.

	self assert: presenter validationLabel label equals: ''.
	self assert: presenter buttonOk isEnabled.
	self deny: presenter changes isEmptyOrNil
]

{ #category : 'tests' }
StRenameClassPreviewPresenterTest >> testCorrectingShadowingNameHidesWarning [

	| driver presenter |
	driver := ReRenameClassDriver2 new
		oldName: #ReClassToBeRenamedChild;
		scopes: { RBBrowserEnvironment default };
		configureRefactoring;
		yourself.
	presenter := self newPresenterFor: driver.
	presenter defaultLayout.
	presenter newNameInput text: 'SharedVar'.
	self assert: (presenter topLayout children includes: presenter behaviorPanel).

	presenter newNameInput text: 'IrisRenamedClass'.

	self deny: (presenter topLayout children includes: presenter behaviorPanel).
	self assert: presenter buttonOk isEnabled
]

{ #category : 'tests' }
StRenameClassPreviewPresenterTest >> testInvalidNameClearsPreviousPreviewAndDisablesApply [

	| presenter |
	presenter := self newPresenterFor: self newDriver.
	presenter newNameInput text: 'IrisRenamedClass'.
	self assert: presenter buttonOk isEnabled.
	self deny: presenter changes isEmptyOrNil.

	presenter newNameInput text: 'Object'.

	self deny: presenter validationLabel label isEmpty.
	self deny: presenter buttonOk isEnabled.
	self assert: presenter changes isEmptyOrNil
]

{ #category : 'tests' }
StRenameClassPreviewPresenterTest >> testRestoringOriginalNameDiscardsPreviousPreview [

	| presenter |
	presenter := self newPresenterFor: self newDriver.
	presenter newNameInput text: 'IrisRenamedClass'.
	self assert: presenter buttonOk isEnabled.
	self deny: presenter changes isEmptyOrNil.

	presenter newNameInput text: 'ReClassToBeRenamed'.

	self assert: presenter validationLabel label equals: ''.
	self deny: presenter buttonOk isEnabled.
	self assert: presenter changes isEmptyOrNil
]

{ #category : 'tests' }
StRenameClassPreviewPresenterTest >> testTypingValidNameUpdatesPreviewImmediately [

	| presenter |
	presenter := self newPresenterFor: self newDriver.
	presenter newNameInput text: 'IrisRenamedClass'.

	self assert: presenter buttonOk isEnabled.
	self assert: presenter name equals: 'IrisRenamedClass'.
	self deny: presenter changes isEmptyOrNil.
	self assert: (presenter changes anySatisfy: [ :change |
		change class = ReRenameClassChange and: [
			change oldName = #ReClassToBeRenamed and: [
				change newName = #IrisRenamedClass ] ] ])
]
