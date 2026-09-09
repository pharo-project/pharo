"
I am a refactoring for a pool  variable addition. My changes are executed on the instance side.
"
Class {
	#name : 'ReAddPoolVariableChange',
	#superclass : 'ReVariableChange',
	#category : 'Refactoring-Changes',
	#package : 'Refactoring-Changes'
}

{ #category : 'converting' }
ReAddPoolVariableChange >> asUndoOperation [

	^ changeFactory
		  removePoolVariable: self variable
		  from: self changeClass
]

{ #category : 'accessing' }
ReAddPoolVariableChange >> changeClass: aBehavior [

	super changeClass: aBehavior.
	isMeta := false
]

{ #category : 'printing' }
ReAddPoolVariableChange >> changeString [

	^ 'Add pool variable <1s> to <2s>'
		  expandMacrosWith: self variable
		  with: self displayClassName
]

{ #category : 'private' }
ReAddPoolVariableChange >> changeSymbol [

	^ #addSharedPoolNamed:
]
