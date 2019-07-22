Class {
	#name : #SpTreeMultipleSelectionMode,
	#superclass : #SpAbstractTreeSingleSelectionMode,
	#category : #'Spec2-Core-Widgets-Tree'
}

{ #category : #testing }
SpTreeMultipleSelectionMode >> isMultipleSelection [

	^ true
]

{ #category : #accessing }
SpTreeMultipleSelectionMode >> presenter: aTreeTablePresenter [ 

	presenter := aTreeTablePresenter 
]

{ #category : #selection }
SpTreeMultipleSelectionMode >> replaceOtherSelectionMode: anotherSelectionMode [

	| originalValueHolder |

	selection := anotherSelectionMode selectedPath.
	originalValueHolder := anotherSelectionMode observablePropertyNamed: #selection.
	
	originalValueHolder announcer subscriptions subscriptions do: [ 
		self halt.	
	]
]

{ #category : #selection }
SpTreeMultipleSelectionMode >> selectPath: aPath [ 
	
	(selection includes: aPath) ifTrue: [ ^ self ].
	presenter itemAtPath: aPath ifAbsent: [ ^ self ].
	
	selection := selection copyWith: aPath.
]

{ #category : #selection }
SpTreeMultipleSelectionMode >> selectPaths: pathArray [
	pathArray
		do: [ :path | presenter itemAtPath: path ifAbsent: [ ^ self ] ].
	selection := pathArray
]

{ #category : #accessing }
SpTreeMultipleSelectionMode >> selectedItem [
	self shouldBeImplemented.
]

{ #category : #accessing }
SpTreeMultipleSelectionMode >> selectedItems [
	selection ifEmpty: [ ^ nil ].
	^ selection collect: [ :path | presenter itemAtPath: path ]
]

{ #category : #accessing }
SpTreeMultipleSelectionMode >> selectedPaths [
	^ selection
]
