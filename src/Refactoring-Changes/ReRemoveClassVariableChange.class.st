"
I am a refactoring for removing a class variable. My changes are executed on the instance side.
"
Class {
	#name : 'ReRemoveClassVariableChange',
	#superclass : 'ReVariableChange',
	#category : 'Refactoring-Changes',
	#package : 'Refactoring-Changes'
}

{ #category : 'converting' }
ReRemoveClassVariableChange >> asUndoOperation [

	^ changeFactory addClassVariable: self variable to: self changeClass
]

{ #category : 'accessing' }
ReRemoveClassVariableChange >> changeClass: aBehavior [

	super changeClass: aBehavior.
	isMeta := false
]

{ #category : 'printing' }
ReRemoveClassVariableChange >> changeString [

	^ 'Remove class variable <1s> from <2s>'
		  expandMacrosWith: self variable
		  with: self displayClassName
]

{ #category : 'private' }
ReRemoveClassVariableChange >> changeSymbol [

	^ #removeClassVarNamed:
]
