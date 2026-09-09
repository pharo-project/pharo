"
I am a refactoring for a class variable addition. My changes are executed on the instance side.
"
Class {
	#name : 'ReAddClassVariableChange',
	#superclass : 'ReVariableChange',
	#category : 'Refactoring-Changes',
	#package : 'Refactoring-Changes'
}

{ #category : 'converting' }
ReAddClassVariableChange >> asUndoOperation [

	^ changeFactory
		  removeClassVariable: self variable
		  from: self changeClass
]

{ #category : 'accessing' }
ReAddClassVariableChange >> changeClass: aBehavior [

	super changeClass: aBehavior.
	isMeta := false
]

{ #category : 'printing' }
ReAddClassVariableChange >> changeString [

	^ 'Add class variable <1s> to <2s>'
		  expandMacrosWith: self variable
		  with: self displayClassName
]

{ #category : 'private' }
ReAddClassVariableChange >> changeSymbol [

	^ #addClassVarNamed:
]
