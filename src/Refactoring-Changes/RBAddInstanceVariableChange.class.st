"
I am a refactoring for a instance  variable addition. My changes are executed on the instance or class side.
"
Class {
	#name : #RBAddInstanceVariableChange,
	#superclass : #RBRefactoryVariableChange,
	#category : #'Refactoring-Changes'
}

{ #category : #converting }
RBAddInstanceVariableChange >> asUndoOperation [
	^ changeFactory removeInstanceVariable: self variable from: self changeClass
]

{ #category : #printing }
RBAddInstanceVariableChange >> changeString [
	^ 'Add instance variable <1s> to <2s>'
		expandMacrosWith: self variable
		with: self displayClassName
]

{ #category : #private }
RBAddInstanceVariableChange >> changeSymbol [
	^ #addInstVarNamed:
]
