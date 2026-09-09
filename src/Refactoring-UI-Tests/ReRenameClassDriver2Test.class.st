"
A ReRenameClassDriver2Test is a test class for testing the behavior of ReRenameClassDriver2
"
Class {
	#name : 'ReRenameClassDriver2Test',
	#superclass : 'TestCase',
	#category : 'Refactoring-UI-Tests-Driver',
	#package : 'Refactoring-UI-Tests',
	#tag : 'Driver'
}

{ #category : 'tests' }
ReRenameClassDriver2Test >> testDetectsBehaviorPreservingPrecondition [
    "Verify that the driver detects a behavior-preserving precondition violation."

    | driver |

    driver := ReRenameClassDriver2 new
        oldName: #ReClassToBeRenamedChild;
        newName: #SharedVar;
        scopes: { RBBrowserEnvironment default };
        configureRefactoring.

    driver createAndSetNewRefactoring.

    self assert: driver hasFailedBehaviorPreservingPreconditions.
]

{ #category : 'tests' }
ReRenameClassDriver2Test >> testInvalidNewClassNameFails [
"Verify that renaming a class to an invalid class name fails the applicability preconditions."

	| driver refactoring |

	driver := ReRenameClassDriver2 new.
	driver
		oldName: #ReClassToBeRenamed;
		newName: #newClass;
		scopes: { RBBrowserEnvironment default };
		configureRefactoring.

	refactoring := driver instantiateRefactoring.
	refactoring newName: driver newName .

	self
		should: [ refactoring checkApplicabilityPreconditions ]
		raise: Error
]

{ #category : 'tests' }
ReRenameClassDriver2Test >> testValidRenameClassPreconditions [
"Verify that renaming a class to a valid name passes the applicability preconditions."

	| driver refactoring |

	driver := ReRenameClassDriver2 new.
	driver
		oldName: #ReClassToBeRenamed;
		newName: #NewName;
		scopes: { RBBrowserEnvironment default };
		configureRefactoring.

	refactoring := driver instantiateRefactoring.
	refactoring newName: driver newName .

	self assert: driver checkApplicabilityPreconditions isNil
]
