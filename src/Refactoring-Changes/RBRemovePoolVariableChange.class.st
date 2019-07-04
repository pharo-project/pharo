"
I am a refactoring for removing a pool variable. My changes are executed on the instance side.
"
Class {
	#name : #RBRemovePoolVariableChange,
	#superclass : #RBRefactoryVariableChange,
	#category : #'Refactoring-Changes'
}

{ #category : #converting }
RBRemovePoolVariableChange >> asUndoOperation [
	^ changeFactory addPoolVariable: self variable to: self changeClass
]

{ #category : #accessing }
RBRemovePoolVariableChange >> changeClass: aBehavior [
	super changeClass: aBehavior.
	isMeta := false
]

{ #category : #printing }
RBRemovePoolVariableChange >> changeString [
	^ 'Remove pool variable <1s> from <2s>' 
		expandMacrosWith: self variable
		with: self displayClassName
]

{ #category : #private }
RBRemovePoolVariableChange >> changeSymbol [
	^ #removeSharedPool:
]
