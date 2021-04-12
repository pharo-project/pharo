"
I am the state of a selected tab
"
Class {
	#name : #TabSelectedState,
	#superclass : #TabEnabled,
	#category : #'Morphic-Widgets-Tabs-States'
}

{ #category : #protocol }
TabSelectedState >> backgroundColor [
	^ Smalltalk ui theme selectionColor
]

{ #category : #protocol }
TabSelectedState >> isSelectedMatchingTheState: aBoolean [

	^ aBoolean
]

{ #category : #protocol }
TabSelectedState >> newStateForSelected: aBoolean [

	^ TabEnabled tab: self tab
]

{ #category : #protocol }
TabSelectedState >> selected [

	^ true
]
