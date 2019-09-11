Class {
	#name : #SpTreeSingleSelectionMode,
	#superclass : #SpAbstractTreeSingleSelectionMode,
	#category : #'Spec2-Core-Widgets-Tree'
}

{ #category : #accessing }
SpTreeSingleSelectionMode >> isMultipleSelection [

	^ false
]

{ #category : #accessing }
SpTreeSingleSelectionMode >> presenter: aTreeTablePresenter [ 

	presenter := aTreeTablePresenter
]

{ #category : #accessing }
SpTreeSingleSelectionMode >> selectPath: aPath [
	"If the path is out of range, keep the selection."
	presenter itemAtPath: aPath ifAbsent: [ ^ self ].
	selection := aPath.

]

{ #category : #accessing }
SpTreeSingleSelectionMode >> selectPaths: pathArray [
	self selectPath: pathArray last.
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
