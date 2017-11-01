"
I am the state of a tab enabled but not yet selected
"
Class {
	#name : #TabEnabled,
	#superclass : #TabState,
	#category : 'Morphic-Widgets-Tabs-States'
}

{ #category : #protocol }
TabEnabled >> backgroundColor [
	^ Smalltalk ui theme darkBaseColor 
]

{ #category : #protocol }
TabEnabled >> enabled [

	^ true
]

{ #category : #protocol }
TabEnabled >> isEnabledMatchingTheState: aBoolean [

	^ aBoolean
]

{ #category : #protocol }
TabEnabled >> isSelectedMatchingTheState: aBoolean [

	^ aBoolean not
]

{ #category : #protocol }
TabEnabled >> newStateForEnabled: aBoolean [
	"aBoolean here must be false"
	self deny: aBoolean.
	
	^ TabDisabled tab: self tab
]

{ #category : #protocol }
TabEnabled >> newStateForSelected: aBoolean [
	self assert: aBoolean.
	
	^ TabSelected tab: self tab
]
