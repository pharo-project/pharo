"
I am a refactoring for a pool  variable addition. My changes are executed on the instance side.
"
Class {
	#name : #RBAddPoolVariableChange,
	#superclass : #RBRefactoryVariableChange,
	#category : #'Refactoring-Changes'
}

{ #category : #converting }
RBAddPoolVariableChange >> asUndoOperation [
	^ changeFactory removePoolVariable: self variable from: self changeClass
]

{ #category : #accessing }
RBAddPoolVariableChange >> changeClass: aBehavior [
	super changeClass: aBehavior.
	isMeta := false
]

{ #category : #printing }
RBAddPoolVariableChange >> changeString [
	^ 'Add pool variable <1s> to <2s>' 
		expandMacrosWith: self variable
		with: self displayClassName
]

{ #category : #private }
RBAddPoolVariableChange >> changeSymbol [
	^ #addSharedPool:
]
