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

{ #category : 'helpers' }
StRenameClassPreviewPresenterTest >> newDriver [

	^ ReRenameClassDriver2 new
		oldName: #ReClassToBeRenamed;
		scopes: { RBBrowserEnvironment default };
		configureRefactoring;
		yourself
]

{ #category : 'helpers' }
StRenameClassPreviewPresenterTest >> newPresenterFor: aDriver [

	^ StControlledRenameClassPreviewPresenter new
		driver: aDriver;
		scopes: { RBBrowserEnvironment default };
		name: aDriver oldName asString;
		yourself
]

{ #category : 'tests' }
StRenameClassPreviewPresenterTest >> testClearingNameClearsErrorAndDisablesApply [

	| presenter |
	presenter := self newPresenterFor: self newDriver.
	presenter newNameInput text: 'Object'.
	self finishPreview: presenter.
	self deny: presenter validationLabel label isEmpty.

	presenter newNameInput text: ''.
	self finishPreview: presenter.

	self assert: presenter validationLabel label equals: ''.
	self deny: presenter buttonOk isEnabled.
	self assert: presenter changes isEmptyOrNil
]

{ #category : 'tests' }
StRenameClassPreviewPresenterTest >> testCorrectingInvalidNameClearsErrorAndEnablesApply [

	| presenter |
	presenter := self newPresenterFor: self newDriver.
	presenter newNameInput text: 'Object'.
	self finishPreview: presenter.
	self deny: presenter validationLabel label isEmpty.
	self deny: presenter buttonOk isEnabled.

	presenter newNameInput text: 'IrisRenamedClass'.
	self finishPreview: presenter.

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
	self finishPreview: presenter.
	self assert: (presenter topLayout children includes: presenter behaviorPanel).

	presenter newNameInput text: 'IrisRenamedClass'.
	self finishPreview: presenter.

	self deny: (presenter topLayout children includes: presenter behaviorPanel).
	self assert: presenter buttonOk isEnabled
]

{ #category : 'tests' }
StRenameClassPreviewPresenterTest >> testInvalidNameClearsPreviousPreviewAndDisablesApply [

	| presenter |
	presenter := self newPresenterFor: self newDriver.
	presenter newNameInput text: 'IrisRenamedClass'.
	self finishPreview: presenter.
	self assert: presenter buttonOk isEnabled.
	self deny: presenter changes isEmptyOrNil.

	presenter newNameInput text: 'Object'.
	self finishPreview: presenter.

	self deny: presenter validationLabel label isEmpty.
	self deny: presenter buttonOk isEnabled.
	self assert: presenter changes isEmptyOrNil
]

{ #category : 'tests' }
StRenameClassPreviewPresenterTest >> testRestoringOriginalNameDiscardsPreviousPreview [

	| presenter |
	presenter := self newPresenterFor: self newDriver.
	presenter newNameInput text: 'IrisRenamedClass'.
	self finishPreview: presenter.
	self assert: presenter buttonOk isEnabled.
	self deny: presenter changes isEmptyOrNil.

	presenter newNameInput text: 'ReClassToBeRenamed'.
	self finishPreview: presenter.

	self assert: presenter validationLabel label equals: ''.
	self deny: presenter buttonOk isEnabled.
	self assert: presenter changes isEmptyOrNil.

	"A completion queued before restoring the original name must also stay hidden."
	presenter newNameInput text: 'IrisAnotherName'.
	presenter computeNextPreview.
	presenter newNameInput text: 'ReClassToBeRenamed'.
	presenter publishNextPreview.
	self deny: presenter isPreviewPending.
	self assert: presenter validationLabel label equals: ''.
	self deny: presenter buttonOk isEnabled.
	self assert: presenter changes isEmptyOrNil

]

{ #category : 'tests' }
StRenameClassPreviewPresenterTest >> testTypingValidNameGeneratesRenamePreview [

	| presenter |
	presenter := StRenameClassPreviewPresenter new
		driver: self newDriver;
		scopes: { RBBrowserEnvironment default };
		name: 'ReClassToBeRenamed';
		yourself.
	presenter newNameInput text: 'IrisRenamedClass'.
	self waitForEnabledPreview: presenter.

	self assert: presenter buttonOk isEnabled.
	self assert: presenter name equals: 'IrisRenamedClass'.
	self deny: presenter changes isEmptyOrNil.
	self assert: (presenter changes anySatisfy: [ :change |
		change class = ReRenameClassChange and: [
			change oldName = #ReClassToBeRenamed and: [
				change newName = #IrisRenamedClass ] ] ])
]

{ #category : 'helpers' }
StRenameClassPreviewPresenterTest >> waitForEnabledPreview: presenter [

	| deadline |
	deadline := Time millisecondClockValue + 5000.
	[ presenter buttonOk isEnabled not and: [
		Time millisecondClockValue < deadline ] ] whileTrue: [ 10 milliSeconds wait ].
	self assert: presenter buttonOk isEnabled
]

{ #category : 'helpers' }
StRenameClassPreviewPresenterTest >> controlledPresenter [

	^ self newPresenterFor: self newDriver
]

{ #category : 'tests' }
StRenameClassPreviewPresenterTest >> testLatestPendingNameWinsAndOlderCompletionIsDiscarded [

	| presenter |
	presenter := self controlledPresenter.
	presenter newNameInput text: 'IrisFirstName'.
	presenter computeNextPreview.
	"Only the latest pending name should be computed after the first result is discarded."
	presenter newNameInput text: 'IrisIntermediateName'.
	presenter newNameInput text: 'IrisFinalName'.
	self deny: presenter buttonOk isEnabled.
	self assert: presenter queuedComputations equals: 0.
	presenter publishNextPreview.
	self deny: presenter buttonOk isEnabled.
	self assert: presenter changes isEmptyOrNil.
	self assert: presenter queuedComputations equals: 1.
	presenter computeNextPreview.
	presenter publishNextPreview.

	self assert: presenter name equals: 'IrisFinalName'.
	self assert: presenter queuedComputations equals: 0.
	self assert: presenter buttonOk isEnabled.
	self assert: (presenter changes anySatisfy: [ :change |
		change class = ReRenameClassChange and: [ change newName = #IrisFinalName ] ])
]

{ #category : 'tests' }
StRenameClassPreviewPresenterTest >> testPendingPreviewCannotBeAccepted [

	| presenter |
	presenter := self controlledPresenter.
	presenter newNameInput text: 'IrisFirstName'.
	presenter accept.
	self assert: presenter isPreviewPending.
	self deny: presenter buttonOk isEnabled
]

{ #category : 'tests' }
StRenameClassPreviewPresenterTest >> testClosingDiscardsQueuedCompletion [

	| presenter |
	presenter := self controlledPresenter.
	presenter newNameInput text: 'IrisFirstName'.
	presenter computeNextPreview.
	presenter closeWindow.
	presenter publishNextPreview.
	self deny: presenter isPreviewPending.
	self deny: presenter buttonOk isEnabled.
	self assert: presenter changes isEmptyOrNil
]

{ #category : 'tests' }
StRenameClassPreviewPresenterTest >> testPublicationFailureLeavesApplyDisabledAndAllowsRecovery [

	| presenter brokenResult |
	presenter := self controlledPresenter.
	presenter newNameInput text: 'IrisFirstName'.
	brokenResult := ReClassRenamePreview new
		driver: self newDriver;
		compositeChange: Object new;
		yourself.
	presenter publishPreview: brokenResult named: 'IrisFirstName' request: presenter currentRequest.
	self deny: presenter isPreviewPending.
	self deny: presenter buttonOk isEnabled.
	self deny: presenter validationLabel label isEmpty.
	presenter newNameInput text: 'IrisFinalName'.
	presenter computeNextPreview; publishNextPreview; computeNextPreview; publishNextPreview.
	self assert: presenter buttonOk isEnabled.
	self assert: presenter validationLabel label equals: ''
]

{ #category : 'helpers' }
StRenameClassPreviewPresenterTest >> finishPreview: presenter [

	presenter isPreviewPending ifTrue: [
		presenter computeNextPreview; publishNextPreview ].
	self deny: presenter isPreviewPending
]
