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
ReRenameClassDriver2Test >> testChangesAfterReconfiguration [

	| driver firstRefactoring firstChanges secondChanges |

	driver := ReRenameClassDriver2 new
		oldName: #ReClassToBeRenamed;
		newName: #NewName;
		scopes: { RBBrowserEnvironment default };
		yourself.

	driver createAndSetNewRefactoring.
	firstRefactoring := driver refactoring.
	firstChanges := driver changes.

	driver newName: #AnotherNewName.
	driver createAndSetNewRefactoring.
	secondChanges := driver changes.

	self deny: firstRefactoring identicalTo: driver refactoring.
	self deny: firstChanges changes isEmpty.
	self deny: secondChanges changes isEmpty.
	self deny: firstChanges identicalTo: secondChanges.

	self assert: (firstChanges changes anySatisfy: [ :change |
		change class = ReRenameClassChange and: [
			change newName = #NewName ] ]).

	self assert: (secondChanges changes anySatisfy: [ :change |
		change class = ReRenameClassChange and: [
			change newName = #AnotherNewName ] ])
]
{ #category : 'tests' }
ReRenameClassDriver2Test >> testChangesCanBeRequestedTwice [

	| driver firstChanges secondChanges |

	driver := ReRenameClassDriver2 new
		oldName: #ReClassToBeRenamed;
		newName: #NewName;
		scopes: { RBBrowserEnvironment default };
		yourself.

	driver createAndSetNewRefactoring.

	firstChanges := driver changes.
	secondChanges := driver changes.

	self deny: firstChanges changes isEmpty.
	self assert: firstChanges identicalTo: secondChanges
]
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

	| driver |

	driver := ReRenameClassDriver2 new.
	driver
		oldName: #ReClassToBeRenamed;
		newName: #newClass;
		scopes: { RBBrowserEnvironment default };
		configureRefactoring.

	driver createAndSetNewRefactoring.

	self deny: driver applicabilityPreconditionsAreSatisfied.
	self deny: driver checkApplicabilityPreconditions isEmptyOrNil 
]

{ #category : 'tests' }
ReRenameClassDriver2Test >> testValidRenameClassPreconditions [
"Verify that renaming a class to a valid name passes the applicability preconditions."

	| driver |

	driver := ReRenameClassDriver2 new.
	driver
		oldName: #ReClassToBeRenamed;
		newName: #NewName;
		scopes: { RBBrowserEnvironment default };
		configureRefactoring.

	driver createAndSetNewRefactoring.

	self assert: driver applicabilityPreconditionsAreSatisfied 
]
