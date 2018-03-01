Extension { #name : #Object }

{ #category : #'*Morphic-Base' }
Object >> addModelYellowButtonMenuItemsTo: aCustomMenu forMorph: aMorph hand: aHandMorph [ 
	"The receiver serves as the model for aMorph; a menu is being constructed for the morph, and here the receiver is able to add its own items"
	Morph cmdGesturesEnabled ifTrue: [ 
		aCustomMenu add: 'inspect model'  target: self selector: #inspect.
	].
	^aCustomMenu

]

{ #category : #'*Morphic-Base' }
Object >> asAlphaImageMorph [

	^ self asMorph
]

{ #category : #'*Morphic-Base' }
Object >> asDraggableMorph [
	^ self asStringMorph 
]

{ #category : #'*Morphic-Base' }
Object >> asMorph [
	"Open a morph, as best one can, on the receiver"

	^ self asStringMorph

	"
234 asMorph.
(MenuIcons tinyMenuIcon) asMorph.
'fred' asMorph.
"


]

{ #category : #'*Morphic-Base' }
Object >> asStringMorph [
	"Open a StringMorph, as best one can, on the receiver"

	^ self asString asStringMorph

]

{ #category : #'*Morphic-Base' }
Object >> asTextMorph [
	"Open a TextMorph, as best one can, on the receiver"

	^ TextMorph new contentsAsIs: self asStringOrText

]

{ #category : #'*Morphic-Base' }
Object >> currentEvent [
	"Answer the current Morphic event.  This method never returns nil."
	^ActiveEvent ifNil:[self currentHand lastEvent]
]

{ #category : #'*Morphic-Base' }
Object >> currentHand [
	"Return a usable HandMorph -- the one associated with the object's current environment.  This method will always return a hand, even if it has to conjure one up as a last resort.  If a particular hand is actually handling events at the moment (such as a remote hand or a ghost hand), it will be returned."

	^ActiveHand ifNil: [ self currentWorld primaryHand ]
]

{ #category : #'*Morphic-Base' }
Object >> defaultLabel [
	"Answer the default label to be used in morphs."

	^ self class name
]

{ #category : #'*Morphic-Base' }
Object >> dragPassengersFor: item inMorph: dragSource [ 

	^ { item }
]

{ #category : #'*Morphic-Base' }
Object >> externalName [
	"Answer an external name by which the receiver is known. Generic
	implementation here is a transitional backstop. probably"
	^ [(self asString copyWithout: Character cr)
		truncateTo: 27]
		ifError: [ ^ self class name printString] 
]

{ #category : #'*Morphic-Base' }
Object >> hasModelYellowButtonMenuItems [
	^Morph cmdGesturesEnabled
]

{ #category : #'*Morphic-Base' }
Object >> iconOrThumbnailOfSize: aNumberOrPoint [ 
	"Answer an appropiate form to represent the receiver"
	^ nil
]

{ #category : #'*Morphic-Base' }
Object >> isTransferable [

	^ false
]

{ #category : #'*Morphic-Base' }
Object >> taskbarIcon [
	"Answer the icon for the receiver in a task bar
	or nil for the default."

	^self class taskbarIcon
]

{ #category : #'*Morphic-Base' }
Object class >> taskbarIcon [
	"Answer the icon for an instance of the receiver in a task bar"

	^self iconNamed: self taskbarIconName
]

{ #category : #'*Morphic-Base' }
Object class >> taskbarIconName [
	"Answer the icon for an instance of the receiver in a task bar"

	^#smallWindowIcon
]

{ #category : #'*Morphic-Base' }
Object >> transferFor: passenger from: aMorph [

	^ TransferMorph withPassenger: passenger from: aMorph
]

{ #category : #'*Morphic-Base' }
Object >> treeNodeHead [

	^ self
]

{ #category : #'*Morphic-Base' }
Object >> treeNodeTail [

	^ nil
]

{ #category : #'*Morphic-Base' }
Object >> wantsVisualFeedback [

	^ true
]
