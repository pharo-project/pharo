"
I am a refactoring for removing an instance variable. My changes are executed on the instance or class side.
"
Class {
	#name : #RBRemoveInstanceVariableChange,
	#superclass : #RBRefactoryVariableChange,
	#category : #'Refactoring-Changes'
}

{ #category : #converting }
RBRemoveInstanceVariableChange >> asUndoOperation [
	^ changeFactory addInstanceVariable: self variable to: self changeClass
]

{ #category : #printing }
RBRemoveInstanceVariableChange >> changeString [
	^ 'Remove instance variable <1s> from <2s>' 
		expandMacrosWith: self variable
		with: self displayClassName
]

{ #category : #private }
RBRemoveInstanceVariableChange >> changeSymbol [
	^ #removeInstVarNamed:
]
