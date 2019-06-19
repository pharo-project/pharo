Class {
	#name : #TreeMultipleSelectionMode,
	#superclass : #AbstractTreeSingleSelectionMode,
	#category : #'Spec-Core-Widgets-Tree'
}

{ #category : #testing }
TreeMultipleSelectionMode >> isMultipleSelection [

	^ true
]

{ #category : #accessing }
TreeMultipleSelectionMode >> presenter: aTreeTablePresenter [ 

	presenter := aTreeTablePresenter 
]

{ #category : #selection }
TreeMultipleSelectionMode >> replaceOtherSelectionMode: anotherSelectionMode [

	| originalValueHolder |

	selection := anotherSelectionMode selectedPath.
	originalValueHolder := anotherSelectionMode observablePropertyNamed: #selection.
	
	originalValueHolder announcer subscriptions subscriptions do: [ 
		self halt.	
	]
]

{ #category : #selection }
TreeMultipleSelectionMode >> selectPath: aPath [ 
	
	(selection includes: aPath) ifTrue: [ ^ self ].
	presenter itemAtPath: aPath ifAbsent: [ ^ self ].
	
	selection := selection copyWith: aPath.
]

{ #category : #selection }
TreeMultipleSelectionMode >> selectPaths: pathArray [
	pathArray
		do: [ :path | presenter itemAtPath: path ifAbsent: [ ^ self ] ].
	selection := pathArray
]

{ #category : #accessing }
TreeMultipleSelectionMode >> selectedItem [
	self shouldBeImplemented.
]

{ #category : #accessing }
TreeMultipleSelectionMode >> selectedItems [
	selection ifEmpty: [ ^ nil ].
	^ selection collect: [ :path | presenter itemAtPath: path ]
]

{ #category : #accessing }
TreeMultipleSelectionMode >> selectedPaths [
	^ selection
]
