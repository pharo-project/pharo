"
I am the state corresponding to a tab being disabled.
"
Class {
	#name : #TabDisabled,
	#superclass : #TabState,
	#category : #'Morphic-Widgets-Tabs-States'
}

{ #category : #protocol }
TabDisabled >> backgroundColor [
	^ Smalltalk ui theme disabledColor
]

{ #category : #protocol }
TabDisabled >> isEnabledMatchingTheState: aBoolean [

	^ aBoolean not
]

{ #category : #protocol }
TabDisabled >> isSelectedMatchingTheState: aBoolean [

	^ aBoolean not
]

{ #category : #protocol }
TabDisabled >> newStateForEnabled: aBoolean [

	^ TabEnabled tab: self tab
]

{ #category : #protocol }
TabDisabled >> newStateForSelected: aBoolean [
	"An disabled tab can not be selected"
	
	^ self
]
