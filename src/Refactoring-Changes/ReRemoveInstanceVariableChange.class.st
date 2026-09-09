"
I am a refactoring for removing an instance variable. My changes are executed on the instance or class side.
"
Class {
	#name : 'ReRemoveInstanceVariableChange',
	#superclass : 'ReVariableChange',
	#category : 'Refactoring-Changes',
	#package : 'Refactoring-Changes'
}

{ #category : 'converting' }
ReRemoveInstanceVariableChange >> asUndoOperation [

	^ changeFactory
		  addInstanceVariable: self variable
		  to: self changeClass
]

{ #category : 'printing' }
ReRemoveInstanceVariableChange >> changeString [

	^ 'Remove instance variable <1s> from <2s>'
		  expandMacrosWith: self variable
		  with: self displayClassName
]

{ #category : 'private' }
ReRemoveInstanceVariableChange >> changeSymbol [

	^ #removeInstVarNamed:
]
