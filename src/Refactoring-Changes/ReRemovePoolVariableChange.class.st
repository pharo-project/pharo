"
I am a refactoring for removing a pool variable. My changes are executed on the instance side.
"
Class {
	#name : 'ReRemovePoolVariableChange',
	#superclass : 'ReVariableChange',
	#category : 'Refactoring-Changes',
	#package : 'Refactoring-Changes'
}

{ #category : 'converting' }
ReRemovePoolVariableChange >> asUndoOperation [

	^ changeFactory addPoolVariable: self variable to: self changeClass
]

{ #category : 'accessing' }
ReRemovePoolVariableChange >> changeClass: aBehavior [

	super changeClass: aBehavior.
	isMeta := false
]

{ #category : 'printing' }
ReRemovePoolVariableChange >> changeString [

	^ 'Remove pool variable <1s> from <2s>'
		  expandMacrosWith: self variable
		  with: self displayClassName
]

{ #category : 'private' }
ReRemovePoolVariableChange >> changeSymbol [

	^ #removeSharedPool:
]
