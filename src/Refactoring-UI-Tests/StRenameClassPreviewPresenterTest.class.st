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

{ #category : 'tests' }
StRenameClassPreviewPresenterTest >> testBehaviorPanelIsHiddenWhenBehaviorPreconditionIsResolved [
    "Verify that the behavior panel is hidden when the behavior-preserving precondition is resolved."

    | driver presenter |

    driver := ReRenameClassDriver2 new
        oldName: #ReClassToBeRenamedChild;
        newName: #SharedVar;
        scopes: { RBBrowserEnvironment default };
        configureRefactoring.

    driver createAndSetNewRefactoring.

    presenter := StRenameClassPreviewPresenter new.
    presenter
        driver: driver;
        scopes: driver scopes.

    presenter defaultLayout.

    presenter updateBehaviorPanelVisibility: 'SharedVar'.

    self assert: (presenter topLayout children includes: presenter behaviorPanel).

    driver newName: #NewClassName.
    driver createAndSetNewRefactoring.

    presenter updateBehaviorPanelVisibility: 'NewClassName'.

    self deny: (presenter topLayout children includes: presenter behaviorPanel)
]

{ #category : 'tests' }
StRenameClassPreviewPresenterTest >> testBehaviorPanelIsShownWhenBehaviorPreconditionFails [
    "Verify that the behavior panel is shown when a behavior-preserving precondition fails."

    | driver presenter |

    driver := ReRenameClassDriver2 new
        oldName: #ReClassToBeRenamedChild;
        newName: #SharedVar;
        scopes: { RBBrowserEnvironment default };
        configureRefactoring.

    driver createAndSetNewRefactoring.

    presenter := StRenameClassPreviewPresenter new.
    presenter
        driver: driver;
        scopes: driver scopes.

    presenter defaultLayout.
    presenter updateBehaviorPanelVisibility: 'SharedVar'.

    self assert: (presenter topLayout children includes: presenter behaviorPanel)
]

{ #category : 'tests' }
StRenameClassPreviewPresenterTest >> testExistingClassNameDisplaysValidationError [
"Verify that entering an existing class name displays a validation error"

    | driver presenter |

    driver := ReRenameClassDriver2 new
        oldName: #ReClassToBeRenamed;
        newName: #Object;
        scopes: { RBBrowserEnvironment default };
        configureRefactoring.

    presenter := StRenameClassPreviewPresenter new.
    presenter
        driver: driver;
        scopes: driver scopes.

    presenter validateName: 'Object'.

    self assert: presenter validationLabel label notEmpty
]

{ #category : 'tests' }
StRenameClassPreviewPresenterTest >> testUpdateChangesWithValidClassNameGeneratesPreview [
"Verify that a valid class name generates the preview changes for the rename refactoring."

    | driver presenter |

	driver := ReRenameClassDriver2 new
   		oldName: #ReClassToBeRenamed;
    	newName: #NewName;
    	scopes: { RBBrowserEnvironment default };
    	configureRefactoring.

	presenter := StRenameClassPreviewPresenter new.
	presenter
    	driver: driver;
    	scopes: driver scopes.

	presenter newNameInput text: 'NewName'.
	presenter validateName: 'NewName'.
	presenter updateName.

	self assert: driver newName equals: 'NewName'.
	self deny: presenter changes isNil


]
