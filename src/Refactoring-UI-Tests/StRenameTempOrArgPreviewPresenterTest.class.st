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
StRenameTempOrArgPreviewPresenterTest >> testInvalidNameUpdatesValidationLabel [
	"Verify that entering an invalid name updates the validation label."

	| driver presenter |
	
	driver := self setUpDriver.

	presenter := StRenameTempOrArgPreviewPresenter new.
	presenter
		driver: driver;
		scopes: { RBBrowserEnvironment default };
		name: 'temp'.

	presenter initializePresenters.

	presenter newNameInput text: 'Tmp2'.
	presenter validateName: 'Tmp2'.

	self assert: presenter validationLabel label
		equals: 'Tmp2 the name musn''t start in uppercase or be a reserved word.'
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
StRenameTempOrArgPreviewPresenterTest >> testValidNameClearsValidationLabel [
	"Verify that entering an invalid name updates the validation label."

	| driver presenter |
	
	driver := self setUpDriver.

	presenter := StRenameTempOrArgPreviewPresenter new.
	presenter
		driver: driver;
		scopes: { RBBrowserEnvironment default };
		name: 'temp'.

	presenter initializePresenters.

	presenter newNameInput text: 'tmp2'.
	presenter validateName: 'tmp2'.

	self assert: presenter validationLabel label equals: ''
]
