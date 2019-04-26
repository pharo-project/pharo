"
I am a refactoring for removing a class variable. My changes are executed on the instance side.
"
Class {
	#name : #RBRemoveClassVariableChange,
	#superclass : #RBRefactoryVariableChange,
	#category : #'Refactoring-Changes'
}

{ #category : #converting }
RBRemoveClassVariableChange >> asUndoOperation [
	^ changeFactory addClassVariable: self variable to: self changeClass
]

{ #category : #accessing }
RBRemoveClassVariableChange >> changeClass: aBehavior [
	super changeClass: aBehavior.
	isMeta := false
]

{ #category : #printing }
RBRemoveClassVariableChange >> changeString [
	^ 'Remove class variable <1s> from <2s>' 
		expandMacrosWith: self variable
		with: self displayClassName
]

{ #category : #private }
RBRemoveClassVariableChange >> changeSymbol [
	^ #removeClassVarNamed:
]
