"
A StRefactoringPreviewPresenter2Test is a test class for testing the behavior of StRefactoringPreviewPresenter2
"
Class {
	#name : 'StRefactoringPreviewPresenter2Test',
	#superclass : 'TestCase',
	#category : 'Refactoring-UI-Tests-UI',
	#package : 'Refactoring-UI-Tests',
	#tag : 'UI'
}

{ #category : 'running' }
StRefactoringPreviewPresenter2Test >> setUpDriver [ 
   | driver method sourceNode |

	 driver := ReRenameArgumentOrTemporaryDriver2 new.

    method := ReClassToConvertTemporaryToInstanceVariable >> #doAction1.

    sourceNode := method ast allChildren
        detect: [ :each | each isVariable and: [ each name = 'tmp1' ] ].

    driver
        sourceNode: sourceNode;
        method: method;
        scopes: { RBBrowserEnvironment default };
	     configureRefactoring .
			
	 ^ driver 
]

{ #category : 'tests' }
StRefactoringPreviewPresenter2Test >> testAcceptAppliesRefactoringChanges [
    "Verify that accepting the preview performs the selected refactoring changes and closes the presenter."

	| driver presenter method sourceNode |

    driver := ReRenameArgumentOrTemporaryDriver2 new.

    method := ReClassToConvertTemporaryToInstanceVariable >> #doAction1.

    sourceNode := method ast allChildren
        detect: [ :each | each isVariable and: [ each name = 'tmp1' ] ].

    driver
        sourceNode: sourceNode;
        method: method;
        scopes: { RBBrowserEnvironment default };
		  configureRefactoring .

    presenter := StRefactoringPreviewPresenterMock lastInstance.
    presenter
        driver: driver;
        scopes: { RBBrowserEnvironment default };
        name: 'tmp1';
	  	  initializePresenters.

	 presenter newNameInput text: 'tmp2'.
	
	 presenter
   		instVarNamed: #activeScope
    	put: RBBrowserEnvironment default.

	presenter
    	instVarNamed: #selectedChanges
    	put: (OrderedCollection with: Object new).

	presenter updateChangesWithName;
			    accept.

	self assert: presenter performed .
	self assert: presenter closed.
]

{ #category : 'tests' }
StRefactoringPreviewPresenter2Test >> testInvalidNameFailsApplicabilityPreconditions [
	"Verify that entering an invalid name fails the applicability preconditions."

	| driver presenter |
	
	driver := self setUpDriver .

	presenter := StRefactoringPreviewPresenter2 new.
	presenter
		driver: driver;
		scopes: { RBBrowserEnvironment default };
		name: 'tmp1'.

	presenter initializePresenters.

	presenter newNameInput text: 'Tmp2'.

	self assert: driver checkApplicabilityPreconditions notNil
]

{ #category : 'accessing' }
StRefactoringPreviewPresenter2Test >> testRunRefactoringCreatesAndConfiguresPresenter [
"Verify that #runRefactoring initializes the preview presenter with the expected data and opens it."

   | driver presenter |

	driver := self setUpDriver .

	driver previewPresenterClass: StRefactoringPreviewPresenterMock ;
			 forTesting: true;
			 runRefactoring.

	presenter := StRefactoringPreviewPresenterMock lastInstance.

	self assert: presenter opened.
	self assert: presenter driver identicalTo: driver.
	self assert: presenter name equals: 'tmp1'.
	self assert: presenter scopes equals: { RBBrowserEnvironment default }.
]

{ #category : 'tests' }
StRefactoringPreviewPresenter2Test >> testUpdateChangesWithValidNameGeneratesPreview [
"Verify that entering a valid name and clicking Rename generates the preview of the refactoring changes."

| driver presenter |

    driver := self setUpDriver .

    presenter := StRefactoringPreviewPresenter2 new.
    presenter
        driver: driver;
        scopes: { RBBrowserEnvironment default };
        name: 'tmp1'.

    presenter initializePresenters.

    presenter newNameInput text: 'tmp2'.

    presenter updateChangesWithName.

    self assert: driver newName equals: 'tmp2'.
	 self deny: driver refactoring changes changes isEmpty.
]
