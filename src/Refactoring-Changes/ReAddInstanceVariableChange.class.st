"
I am a refactoring for a instance  variable addition. My changes are executed on the instance or class side.
"
Class {
	#name : 'ReAddInstanceVariableChange',
	#superclass : 'ReVariableChange',
	#category : 'Refactoring-Changes',
	#package : 'Refactoring-Changes'
}

{ #category : 'converting' }
ReAddInstanceVariableChange >> asUndoOperation [

	^ changeFactory
		  removeInstanceVariable: self variable
		  from: self changeClass
]

{ #category : 'printing' }
ReAddInstanceVariableChange >> changeString [

	^ 'Add instance variable <1s> to <2s>'
		  expandMacrosWith: self variable
		  with: self displayClassName
]

{ #category : 'private' }
ReAddInstanceVariableChange >> changeSymbol [

	^ #addInstVarNamed:
]
