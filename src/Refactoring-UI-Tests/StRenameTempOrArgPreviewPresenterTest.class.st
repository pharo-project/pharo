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
StRenameTempOrArgPreviewPresenterTest >> testAcceptAppliesRefactoringChanges [
    "Verify that accepting the preview performs the selected refactoring changes and closes the presenter."

    | driver presenter method sourceNode |

    driver := ReRenameArgumentOrTemporaryDriver2 new.

    method := ReClassToConvertTemporaryToInstanceVariable >> #doAction1.

    sourceNode := method ast allChildren
        detect: [ :each | each isVariable and: [ each name = 'temp' ] ].

    driver
        sourceNode: sourceNode;
        method: method;
        scopes: { RBBrowserEnvironment default };
        configureRefactoring.

    presenter := StRefactoringPreviewPresenterMock lastInstance.
    presenter
        driver: driver;
        scopes: { RBBrowserEnvironment default };
        name: 'temp';
        initializePresenters.

    presenter
        instVarNamed: #activeScope
        put: RBBrowserEnvironment default.

    presenter newNameInput text: 'tmp2'.
	 presenter validateName: 'tmp2'.
    presenter updateName.
    presenter accept.

    self assert: presenter performed.
    self assert: presenter closed.
]

{ #category : 'tests' }
StRenameTempOrArgPreviewPresenterTest >> testInvalidNameFailsApplicabilityPreconditions [
	"Verify that entering an invalid name fails the applicability preconditions."

	| driver presenter failedCondition |
	
	driver := self setUpDriver .

	presenter := StRenameTempOrArgPreviewPresenter new.
	presenter
		driver: driver;
		scopes: { RBBrowserEnvironment default };
		name: 'temp'.

	presenter initializePresenters.

	presenter newNameInput text: 'Tmp2'.
	presenter validateName: 'Tmp2'.

	failedCondition := driver checkApplicabilityPreconditions .
	
	self assert: failedCondition
		  equals: 'Tmp2 the name musn''t start in uppercase or be a reserved word.'
]

{ #category : 'tests' }
StRenameTempOrArgPreviewPresenterTest >> testRenameTemporaryToInstanceVariableNameFailsBehaviorPreservingPreconditions [
"Verify that renaming a temporary variable to an existing instance/class variable fails the behavior-preserving preconditions."

	| driver message |
	
	driver := self setUpDriver .
	driver newName: 'instVar'.
	
	driver createAndSetNewRefactoring.
	message := driver checkBehaviorPreservingPreconditions.
	
	self assert: message 
		  equals: 'The variable "instVar" is already defined as a class/instance variable in the given context', 	         		String cr
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
StRenameTempOrArgPreviewPresenterTest >> testUpdateChangesWithValidNameGeneratesPreview [
"Verify that entering a valid name generates the preview of the refactoring changes."

| driver presenter |

    driver := self setUpDriver .

    presenter := StRenameTempOrArgPreviewPresenter new.
    presenter
        driver: driver;
        scopes: { RBBrowserEnvironment default };
        name: 'temp'.

    presenter initializePresenters.

    presenter newNameInput text: 'tmp2'.
	 presenter validateName: 'tmp2'.
   
    presenter updateName.

    self assert: driver newName equals: 'tmp2'.
	 self deny: driver refactoring changes changes isEmpty.
]
