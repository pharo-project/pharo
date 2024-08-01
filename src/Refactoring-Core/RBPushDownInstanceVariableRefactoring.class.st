"
I am a refactoring for moving a instance variable down to my subclasses.

My precondition verifies that the moved variable is not referenced in the methods of the original class.
"
Class {
	#name : 'RBPushDownInstanceVariableRefactoring',
	#superclass : 'RBVariableRefactoring',
	#category : 'Refactoring-Core-Refactorings',
	#package : 'Refactoring-Core',
	#tag : 'Refactorings'
}

{ #category : 'preconditions' }
RBPushDownInstanceVariableRefactoring >> applicabilityPreconditions [

	^ { (RBCondition definesInstanceVariable: variableName in: class) }
]

{ #category : 'preconditions' }
RBPushDownInstanceVariableRefactoring >> breakingChangePreconditions [

	| references |
	references := RBCondition
		              referencesInstanceVariable: variableName
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
								 instVarRefsTo: variableName
								 in: class realClass) ] ].
	^ { references not }
]

{ #category : 'preconditions' }
RBPushDownInstanceVariableRefactoring >> preconditions [ 

	^ self applicabilityPreconditions & self breakingChangePreconditions 
]

{ #category : 'transforming' }
RBPushDownInstanceVariableRefactoring >> privateTransform [
	class removeInstanceVariable: variableName.
	class subclasses do: [:each | each addInstanceVariable: variableName]
]
