"
I am a refactoring for moving a class variable down to my subclasses.

My precondition verifies that the moved variable is not referenced in the methods of the original class.
"
Class {
	#name : 'RBPushDownClassVariableRefactoring',
	#superclass : 'RBVariableRefactoring',
	#category : 'Refactoring-Core-Refactorings',
	#package : 'Refactoring-Core',
	#tag : 'Refactorings'
}

{ #category : 'preconditions' }
RBPushDownClassVariableRefactoring >> applicabilityPreconditions [

	^ { (RBCondition definesClassVariable: variableName in: class) }
]

{ #category : 'preconditions' }
RBPushDownClassVariableRefactoring >> breakingChangePreconditions [

	| references |
	references := RBCondition
		              referencesClassVariable: variableName
		              in: class.
	class realClass
		ifNil: [
			references errorMacro:
				('<1s> is referenced.' expandMacrosWith: variableName) ]
		ifNotNil: [
			references
				errorMacro:
					('<1s> is referenced.<n>Browse references?' expandMacrosWith:
							 variableName);
				errorBlock: [
					self openBrowserOn: (RBBrowserEnvironment new
								 classVarRefsTo: variableName
								 in: class realClass) ] ].
	^ { references not }
]

{ #category : 'preconditions' }
RBPushDownClassVariableRefactoring >> findDestinationClasses [

	| classes |
	classes := class withAllSubclasses reject: [ :each |
		(each whichSelectorsReferToClassVariable: variableName) isEmpty
			and: [ (each classSide whichSelectorsReferToClassVariable: variableName) isEmpty ] ].
	^ classes
]

{ #category : 'preconditions' }
RBPushDownClassVariableRefactoring >> preconditions [

	^ self applicabilityPreconditions & self breakingChangePreconditions
]

{ #category : 'transforming' }
RBPushDownClassVariableRefactoring >> privateTransform [

	| destinationClasses |
	class removeClassVariable: variableName.
	destinationClasses := self findDestinationClasses.
	destinationClasses ifEmpty: [ ^ self ].
	destinationClasses do: [ :aClass | aClass addClassVariable: variableName ]
]
