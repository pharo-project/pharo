"
I am a refactoring for a class variable addition. My changes are executed on the instance side.
"
Class {
	#name : #RBAddClassVariableChange,
	#superclass : #RBRefactoryVariableChange,
	#category : #'Refactoring-Changes'
}

{ #category : #converting }
RBAddClassVariableChange >> asUndoOperation [
	^ changeFactory
		removeClassVariable: self variable
		from: self changeClass
]

{ #category : #accessing }
RBAddClassVariableChange >> changeClass: aBehavior [
	super changeClass: aBehavior.
	isMeta := false
]

{ #category : #printing }
RBAddClassVariableChange >> changeString [
	^ 'Add class variable <1s> to <2s>' 
		expandMacrosWith: self variable
		with: self displayClassName
]

{ #category : #private }
RBAddClassVariableChange >> changeSymbol [
	^ #addClassVarNamed:
]
