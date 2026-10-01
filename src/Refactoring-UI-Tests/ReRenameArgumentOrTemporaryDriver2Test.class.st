"
A ReRenameArgumentOrTemporaryDriver2Test is a test class for testing the behavior of ReRenameArgumentOrTemporaryDriver2
"
Class {
	#name : 'ReRenameArgumentOrTemporaryDriver2Test',
	#superclass : 'TestCase',
	#category : 'Refactoring-UI-Tests-Driver',
	#package : 'Refactoring-UI-Tests',
	#tag : 'Driver'
}

{ #category : 'initialization' }
ReRenameArgumentOrTemporaryDriver2Test >> setUpDriver [ 
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
ReRenameArgumentOrTemporaryDriver2Test >> testInvalidNameFailsApplicabilityPreconditions [
	"Verify that entering an invalid name fails the applicability preconditions."

	| driver failedCondition |
	
	driver := self setUpDriver .

	driver newName: 'Tmp2'.
	driver createAndSetNewRefactoring . 

	failedCondition := driver checkApplicabilityPreconditions .
	
	self assert: failedCondition
		  equals: 'Tmp2 the name musn''t start in uppercase or be a reserved word.'
]

{ #category : 'tests' }
ReRenameArgumentOrTemporaryDriver2Test >> testRenameTemporaryToInstanceVariableNameFailsBehaviorPreservingPreconditions [
"Verify that renaming a temporary variable to an existing instance/class variable fails the behavior-preserving preconditions."

	| driver message |
	
	driver := self setUpDriver .
	driver newName: 'instVar'.
	
	driver createAndSetNewRefactoring.
	message := driver checkBehaviorPreservingPreconditions.
	
	self assert: message 
		  equals: 'The variable "instVar" is already defined as a class/instance variable in the given context', 	         		String cr
]
