Class {
	#name : #SpTreeSingleSelectionMode,
	#superclass : #SpAbstractTreeSelectionMode,
	#category : #'Spec2-Core-Widgets-Tree'
}

{ #category : #selection }
SpTreeSingleSelectionMode >> clearSelection [

	selection := #()
]

{ #category : #selection }
SpTreeSingleSelectionMode >> itemNotFoundAction [
	self clearSelection
]

{ #category : #accessing }
SpTreeSingleSelectionMode >> selectPath: aPath [
	"Check comment in my superclass to know how to use selectPath:"

	"If the path is out of range, keep the selection."
	aPath ifEmpty: [ ^ self unselectAll ].
	presenter 
		itemAtPath: aPath 
		ifAbsent: [ ^ self ].
	selection := aPath.

]

{ #category : #accessing }
SpTreeSingleSelectionMode >> selectPaths: pathArray [

	self selectPath: pathArray last
]

{ #category : #accessing }
SpTreeSingleSelectionMode >> selectedItem [
	
	selection ifEmpty: [ ^ nil ].
	
	^ presenter itemAtPath: selection.
]

{ #category : #accessing }
SpTreeSingleSelectionMode >> selectedPath [
	
	^ selection
]

{ #category : #accessing }
SpTreeSingleSelectionMode >> selectedPaths [
	
	selection ifEmpty: [ ^ #() ].
	^ { selection }
]

{ #category : #selecting }
SpTreeSingleSelectionMode >> unselectPath: aPath [
	"If the path is out of range, keep the selection."
	selection = aPath ifFalse: [ ^ self ].
	self clearSelection
]
