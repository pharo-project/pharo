"
A StRefactoringPreviewPresenter2Test is a test class for testing the behavior of StRefactoringPreviewPresenter2
"
Class {
	#name : 'StRenameTempOrArgPreviewPresenterTest',
	#superclass : 'TestCase',
	#category : 'Refactoring-UI-Tests-UI',
	#package : 'Refactoring-UI-Tests',
	#tag : 'UI'
}

{ #category : 'running' }
StRenameTempOrArgPreviewPresenterTest >> newPresenterFor: aDriver [

	^ StRenameTempOrArgPreviewPresenter new
		driver: aDriver;
		scopes: { RBBrowserEnvironment default };
		name: 'temp';
		yourself
]

{ #category : 'running' }
StRenameTempOrArgPreviewPresenterTest >> setUpDriver [ 
   | driver method sourceNode |

	 driver := ReRenameArgumentOrTemporaryDriver2 new.

    method := ReClassToConvertTemporaryToInstanceVariable >> #doAction1.

    sourceNode := method ast allChildren
        detect: [ :each | each isVariable and: [ each name = 'temp' ] ].

    driver
        sourceNode: sourceNode;
        method: method;
        scopes: { RBBrowserEnvironment default };
	     configureRefactoring .
			
	 ^ driver 
]

{ #category : 'tests' }
StRenameTempOrArgPreviewPresenterTest >> testClearingNameClearsErrorAndDisablesApply [

	| presenter |
	presenter := self newPresenterFor: self setUpDriver.
	presenter newNameInput text: 'Tmp2'.
	self deny: presenter validationLabel label isEmpty.

	presenter newNameInput text: ''.

	self assert: presenter validationLabel label equals: ''.
	self deny: presenter buttonOk isEnabled.
	self assert: presenter changes isEmptyOrNil
]

{ #category : 'tests' }
StRenameTempOrArgPreviewPresenterTest >> testCorrectingInvalidNameClearsErrorAndEnablesApply [

	| presenter |
	presenter := self newPresenterFor: self setUpDriver.
	presenter newNameInput text: 'Tmp2'.
	self deny: presenter validationLabel label isEmpty.
	self deny: presenter buttonOk isEnabled.

	presenter newNameInput text: 'renamedTemporary'.

	self assert: presenter validationLabel label equals: ''.
	self assert: presenter buttonOk isEnabled.
	self deny: presenter changes isEmptyOrNil
]

{ #category : 'tests' }
StRenameTempOrArgPreviewPresenterTest >> testInvalidNameClearsPreviousPreviewAndDisablesApply [

	| presenter |
	presenter := self newPresenterFor: self setUpDriver.
	presenter newNameInput text: 'renamedTemporary'.
	self assert: presenter buttonOk isEnabled.
	self deny: presenter changes isEmptyOrNil.

	presenter newNameInput text: 'Tmp2'.

	self deny: presenter validationLabel label isEmpty.
	self deny: presenter buttonOk isEnabled.
	self assert: presenter changes isEmptyOrNil
]

{ #category : 'tests' }
StRenameTempOrArgPreviewPresenterTest >> testRestoringOriginalNameDiscardsPreviousPreview [

	| presenter |
	presenter := self newPresenterFor: self setUpDriver.
	presenter newNameInput text: 'renamedTemporary'.
	self assert: presenter buttonOk isEnabled.
	self deny: presenter changes isEmptyOrNil.

	presenter newNameInput text: 'temp'.

	self assert: presenter validationLabel label equals: ''.
	self deny: presenter buttonOk isEnabled.
	self assert: presenter changes isEmptyOrNil
]

{ #category : 'accessing' }
StRenameTempOrArgPreviewPresenterTest >> testRunRefactoringCreatesAndConfiguresPresenter [
"Verify that #runRefactoring initializes the preview presenter with the expected data and opens it."

   | driver presenter |

	driver := self setUpDriver .

	driver previewPresenterClass: StRefactoringPreviewPresenterMock ;
			 forTesting: true;
			 runRefactoring.

	presenter := StRefactoringPreviewPresenterMock lastInstance.

	self assert: presenter opened.
	self assert: presenter driver identicalTo: driver.
	self assert: presenter name equals: 'temp'.
	self assert: presenter scopes equals: { RBBrowserEnvironment default }.
]

{ #category : 'tests' }
StRenameTempOrArgPreviewPresenterTest >> testTypingValidNameUpdatesPreviewImmediately [

	| presenter renamedMethod |
	presenter := self newPresenterFor: self setUpDriver.
	presenter newNameInput text: 'renamedTemporary'.

	self assert: presenter buttonOk isEnabled.
	self assert: presenter name equals: 'renamedTemporary'.
	self assert: presenter changes size equals: 1.
	renamedMethod := RBParser parseMethod: presenter changes first textToDisplay.
	self assert: (renamedMethod allChildren anySatisfy: [ :node |
		node isVariable and: [ node name = 'renamedTemporary' ] ]).
	self deny: (renamedMethod allChildren anySatisfy: [ :node |
		node isVariable and: [ node name = 'temp' ] ])
]
