"
I am a refactoring for moving a class variable down to my subclasses.

My precondition verifies that the moved variable is not referenced in the methods of the original class.
"
Class {
	#name : 'RePushDownSharedVariableRefactoring',
	#superclass : 'RBVariableRefactoring',
	#category : 'Refactoring-Core-Refactorings',
	#package : 'Refactoring-Core',
	#tag : 'Refactorings'
}

{ #category : 'preconditions' }
RePushDownSharedVariableRefactoring >> applicabilityPreconditions [

	^ {
		  (RBCondition definesClassVariable: variableName in: class).
		  (ReClassesHaveAnySubclassCondition new classes: { class }) }
]

{ #category : 'preconditions' }
RePushDownSharedVariableRefactoring >> breakingChangePreconditions [

	^ { self preconditionHasNoReferences }
]

{ #category : 'preconditions' }
RePushDownSharedVariableRefactoring >> findDestinationClasses [

	| classes |
	classes := class allSubclasses reject: [ :each |
		(each whichSelectorsReferToClassVariable: variableName) isEmpty
			and: [ (each classSide whichSelectorsReferToClassVariable: variableName) isEmpty ] ].
	^ classes
]

{ #category : 'preconditions' }
RePushDownSharedVariableRefactoring >> preconditionHasNoReferences [

	^ (ReSharedVariableHasReferencesInDefiningClass new class: class referencesToSharedVariable: variableName) not
]

{ #category : 'preconditions' }
RePushDownSharedVariableRefactoring >> preconditions [

	^ self applicabilityPreconditions & self breakingChangePreconditions
]

{ #category : 'transforming' }
RePushDownSharedVariableRefactoring >> privateTransform [

	| destinationClasses |

	class removeClassVariable: variableName.
	destinationClasses := self findDestinationClasses.
	destinationClasses ifEmpty: [ ^ self ].
	destinationClasses do: [ :aClass | aClass addClassVariable: variableName ]
]
