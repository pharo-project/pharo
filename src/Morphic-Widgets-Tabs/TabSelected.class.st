"
I am the state of a selected tab
"
Class {
	#name : #TabSelected,
	#superclass : #TabEnabledState,
	#category : #'Morphic-Widgets-Tabs-States'
}

{ #category : #protocol }
TabSelected >> backgroundColor [
	^ Smalltalk ui theme selectionColor
]

{ #category : #protocol }
TabSelected >> isSelectedMatchingTheState: aBoolean [

	^ aBoolean
]

{ #category : #protocol }
TabSelected >> newStateForSelected: aBoolean [

	^ TabEnabledState tab: self tab
]

{ #category : #protocol }
TabSelected >> selected [

	^ true
]
