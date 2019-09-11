"
I am a refactoring for removing instance variables.

My precondition verifies that there is no reference to this instance  variable.
"
Class {
	#name : #RBRemoveInstanceVariableRefactoring,
	#superclass : #RBVariableRefactoring,
	#category : #'Refactoring-Core-Refactorings'
}

{ #category : #'as yet unclassified' }
RBRemoveInstanceVariableRefactoring class >> model: aNamespace remove: variable from: class [
	^ self model: aNamespace variable: variable class: class
]

{ #category : #'as yet unclassified' }
RBRemoveInstanceVariableRefactoring class >> remove: variable from: class [
	^ self variable: variable class: class
]

{ #category : #preconditions }
RBRemoveInstanceVariableRefactoring >> preconditions [

	| references |

	references := RBCondition hierarchyOf: class referencesInstanceVariable: variableName.
	class realClass
		ifNil: [ references errorMacro: ( '<1s> is referenced.' expandMacrosWith: variableName ) ]
		ifNotNil: [ references
				errorMacro: ( '<1s> is referenced.<n>Browse references?' expandMacrosWith: variableName );
				errorBlock:
						[ self openBrowserOn: ( RBBrowserEnvironment new instVarRefsTo: variableName in: class realClass ) ]
			].
	^ ( RBCondition definesInstanceVariable: variableName asString in: class ) & references not
]

{ #category : #transforming }
RBRemoveInstanceVariableRefactoring >> transform [
	class removeInstanceVariable: variableName
]
