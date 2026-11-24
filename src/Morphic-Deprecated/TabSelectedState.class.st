"
I am the state of a selected tab
"
Class {
	#name : 'TabSelectedState',
	#superclass : 'TabEnabledState',
	#category : 'Morphic-Deprecated',
	#package : 'Morphic-Deprecated'
}

{ #category : 'protocol' }
TabSelectedState >> backgroundColor [
	^ Smalltalk ui theme selectionColor
]

{ #category : 'protocol' }
TabSelectedState >> isSelectedMatchingTheState: aBoolean [

	^ aBoolean
]

{ #category : 'protocol' }
TabSelectedState >> newStateForSelected: aBoolean [

	^ TabEnabledState tab: self tab
]

{ #category : 'protocol' }
TabSelectedState >> selected [

	^ true
]
