"
A TickSelectionListMorph is used to render the tick list

storeValue
	- xxxxx

"
Class {
	#name : #TickSelectionListMorph,
	#superclass : #MorphTreeMorph,
	#instVars : [
		'storeValue'
	],
	#category : 'Morphic-Widgets-TickList'
}

{ #category : #event }
TickSelectionListMorph >> basicKeyPressed: aCharacter [

	aCharacter == Character space
		ifTrue: [ ^ self model toogleSelectionValue ].
	
	^ super basicKeyPressed: aCharacter
]

{ #category : #accessing }
TickSelectionListMorph >> indentingItemClass [
	
	^ TickSelectionListNodeMorph
]

{ #category : #accessing }
TickSelectionListMorph >> mouseUp: event [ 

	self storeValue: nil.
	super mouseUp: event 
]

{ #category : #accessing }
TickSelectionListMorph >> storeValue [

	^ storeValue
]

{ #category : #accessing }
TickSelectionListMorph >> storeValue: anObject [
	
	storeValue := anObject
]
