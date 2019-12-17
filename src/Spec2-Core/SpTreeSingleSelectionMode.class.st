Class {
	#name : #SpTreeSingleSelectionMode,
	#superclass : #SpAbstractTreeSingleSelectionMode,
	#category : #'Spec2-Core-Widgets-Tree'
}

{ #category : #accessing }
SpTreeSingleSelectionMode >> selectPath: aPath [
	"If the path is out of range, keep the selection."

	aPath ifEmpty: [ ^ self unselectAll ].
	presenter itemAtPath: aPath ifAbsent: [ ^ self ].
	selection := aPath

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

{ #category : #accessing }
SpTreeSingleSelectionMode >> unselectPath: aPath [
	"If the path is out of range, keep the selection."
	selection = aPath ifFalse: [ ^ self ].
	self clearSelection
]
