"
A TickSelectionListNodeMorph is the morph used to render the tree used to render te tick list
"
Class {
	#name : #TickSelectionListNodeMorph,
	#superclass : #MorphTreeNodeMorph,
	#category : #Morphic-Widgets-TickList
}

{ #category : #drawing }
TickSelectionListNodeMorph >> drawMouseDownHighlightOn: aCanvas [
	"Draw with a dotted border."

	self highlightedForMouseDown 
		ifTrue: [
			container ifNil: [^super drawMouseDownHighlightOn: aCanvas].
			self storeValue ifNil: [ self storeValue: self complexContents selected not ].
			self complexContents selected: self storeValue.
			aCanvas 
				frameRectangle: self selectionFrame
				width: 1
				colors: {container mouseDownHighlightColor. Color transparent}
				dashes: #(1 1)]
]

{ #category : #event }
TickSelectionListNodeMorph >> pressed: anIndex [ 

	self complexContents pressed: anIndex
]

{ #category : #accessing }
TickSelectionListNodeMorph >> storeValue [

	^ self owner owner storeValue
]

{ #category : #accessing }
TickSelectionListNodeMorph >> storeValue: aValue [

	^ self owner owner storeValue: aValue
]
