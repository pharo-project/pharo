"
A ReRenameMethodDriver2Test is a test class for testing the behavior of ReRenameMethodDriver2
"
Class {
	#name : 'ReRenameMethodDriver2Test',
	#superclass : 'TestCase',
	#category : 'Refactoring-UI-Tests-Driver',
	#package : 'Refactoring-UI-Tests',
	#tag : 'Driver'
}

{ #category : 'running' }
ReRenameMethodDriver2Test >> setUpDriver [

	| rbclasses model method |

	rbclasses := RBClassEnvironment classes: { MySubAccessingSuperclassState }.
	model := RBNamespace onEnvironment: rbclasses.
	method := (model classObjectFor: MySubAccessingSuperclassState) methodFor: #accessingSharedVariable.
	^ ReRenameMethodDriver2 new
		          scopes: { rbclasses }
		          model: model
		          renameMethodSignature: method
		          in: MySubAccessingSuperclassState
]

{ #category : 'tests' }
ReRenameMethodDriver2Test >> testRenameMethodToExistingSelectorFailsApplicabilityPreconditions [
"Verify that renaming a method to an existing selector fails the applicability preconditions."

	| driver |

	driver := self setUpDriver .

	driver newName: #accessing .
	driver configureRefactoring.

	self deny: driver applicabilityPreconditionsAreSatisfied
]

{ #category : 'tests' }
ReRenameMethodDriver2Test >> testRenameMethodToInvalidSelectorFailsApplicabilityPreconditions [
"Verify that renaming a method to an invalid selector fails the applicability preconditions."

	| driver |

	driver := self setUpDriver .

	driver newName: '3accessing' asSymbol .
	driver configureRefactoring.

	self deny: driver applicabilityPreconditionsAreSatisfied
]

{ #category : 'tests' }
ReRenameMethodDriver2Test >> testRenameMethodToSelectorDefinedInSuperclassFailsBehaviorPreconditions [
"Verify that renaming a method to a selector defined in a superclass fails the behavior-preserving preconditions."

	| driver message |

	driver := self setUpDriver .

	driver newName: #initialize .
	driver configureRefactoring.
	driver refactoring newName: #initialize.

	message := driver checkBehaviorPreconditions .
	
	self assert: message notNil 
]

{ #category : 'tests' }
ReRenameMethodDriver2Test >> testRenameMethodToSelectorWithDifferentArityFailsApplicabilityPreconditions [
"Verify that renaming a method to a selector with a different arity fails the applicability preconditions."

	| driver |

	driver := self setUpDriver .

	driver newName: #accessingSharedVariable: .
	driver configureRefactoring.

	self deny: driver applicabilityPreconditionsAreSatisfied
]
