Extension { #name : 'Object' }

{ #category : '*Morphic-Widgets' }
Object >> acceptDroppingMorph: transferMorph event: evt inMorph: dstListMorph [

	^false
]

{ #category : '*Morphic-Widgets' }
Object >> addModelYellowButtonMenuItemsTo: aCustomMenu forMorph: aMorph hand: aHandMorph [
	"The receiver serves as the model for aMorph; a menu is being constructed for the morph, and here the receiver is able to add its own items"
	Morph cmdGesturesEnabled ifTrue: [
		aCustomMenu add: 'inspect model'  target: self selector: #inspect.
	].
	^aCustomMenu
]

{ #category : '*Morphic-Widgets' }
Object >> asAlphaImageMorph [

	^ self asMorph
]

{ #category : '*Morphic-Widgets' }
Object >> asDraggableMorph [

	^ self asStringMorph
]

{ #category : '*Morphic-Widgets' }
Object >> asMorph [
	"Open a morph, as best one can, on the receiver"

	^ self asStringMorph
]

{ #category : '*Morphic-Widgets' }
Object >> asStringMorph [
	"Open a StringMorph, as best one can, on the receiver"

	^ self asString asStringMorph
]

{ #category : '*Morphic-Widgets' }
Object >> asTextMorph [
	"Open a TextMorph, as best one can, on the receiver"

	^ TextMorph new contentsAsIs: self asStringOrText
]

{ #category : '*Morphic-Widgets' }
Object >> currentEvent [
	"Answer the current Morphic event.  This method never returns nil."
	^ self currentHand lastEvent
]

{ #category : '*Morphic-Widgets' }
Object >> defaultLabel [
	"Answer the default label to be used in morphs."

	^ self class name
]

{ #category : '*Morphic-Widgets' }
Object >> dragPassengerFor: item inMorph: dragSource [

	^ item
]

{ #category : '*Morphic-Widgets' }
Object >> dragPassengersFor: item inMorph: dragSource [

	^ { item }
]

{ #category : '*Morphic-Widgets' }
Object >> dragTransferType [

	^ nil
]

{ #category : '*Morphic-Widgets' }
Object >> dragTransferTypeForMorph: dragSource [

	^ nil
]

{ #category : '*Morphic-Widgets' }
Object >> iconFormSetNamed: aSymbol [

	^ Smalltalk ui icons iconFormSetNamed: aSymbol
]

{ #category : '*Morphic-Widgets' }
Object >> isTransferable [

	^ false
]

{ #category : '*Morphic-Widgets' }
Object >> okToClose [
	"Sent to models when a window closing.
	Allows this check to be independent of okToChange."

	^true
]

{ #category : '*Morphic-Widgets' }
Object >> reactToRubTextAreaDoubleClick: anEvent [
	"Noop"
]

{ #category : '*Morphic-Widgets' }
Object >> systemIconName [

	^ #class
]

{ #category : '*Morphic-Widgets' }
Object class >> taskbarIconName [
	"Answer the icon for an instance of the receiver in a task bar"

	^#smallWindow
]

{ #category : '*Morphic-Widgets' }
Object >> taskbarIconName [
	"Answer the icon for the receiver in a task bar
	or nil for the default."

	^self class taskbarIconName
]

{ #category : '*Morphic-Widgets' }
Object >> transferFor: passenger from: aMorph [

	^ TransferMorph withPassenger: passenger from: aMorph
]

{ #category : '*Morphic-Widgets' }
Object >> treeNodeHead [

	^ self
]

{ #category : '*Morphic-Widgets' }
Object >> treeNodeTail [

	^ nil
]

{ #category : '*Morphic-Widgets' }
Object >> unplug [
]

{ #category : '*Morphic-Widgets' }
Object >> wantsDroppedMorph: aMorph event: anEvent inMorph: destinationLM [
	^false
]

{ #category : '*Morphic-Widgets' }
Object >> wantsVisualFeedback [

	^ true
]

{ #category : '*Morphic-Widgets' }
Object >> withoutListWrapper [

	^self
]
