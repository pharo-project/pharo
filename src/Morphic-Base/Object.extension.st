Extension { #name : 'Object' }

{ #category : '*Morphic-Base' }
Object >> addModelYellowButtonMenuItemsTo: aCustomMenu forMorph: aMorph hand: aHandMorph [
	"The receiver serves as the model for aMorph; a menu is being constructed for the morph, and here the receiver is able to add its own items"
	Morph cmdGesturesEnabled ifTrue: [
		aCustomMenu add: 'inspect model'  target: self selector: #inspect.
	].
	^aCustomMenu
]

{ #category : '*Morphic-Base' }
Object >> asAlphaImageMorph [

	^ self asMorph
]

{ #category : '*Morphic-Base' }
Object >> asDraggableMorph [

	^ self asStringMorph
]

{ #category : '*Morphic-Base' }
Object >> asMorph [
	"Open a morph, as best one can, on the receiver"

	^ self asStringMorph
]

{ #category : '*Morphic-Base' }
Object >> asStringMorph [
	"Open a StringMorph, as best one can, on the receiver"

	^ self asString asStringMorph
]

{ #category : '*Morphic-Base' }
Object >> asTextMorph [
	"Open a TextMorph, as best one can, on the receiver"

	^ TextMorph new contentsAsIs: self asStringOrText
]

{ #category : '*Morphic-Base' }
Object >> currentEvent [
	"Answer the current Morphic event.  This method never returns nil."
	^ self currentHand lastEvent
]

{ #category : '*Morphic-Base' }
Object >> currentHand [
	"Return a usable HandMorph -- the one associated with the object's current environment.  This method will always return a hand, even if it has to conjure one up as a last resort.  If a particular hand is actually handling events at the moment (such as a remote hand or a ghost hand), it will be returned."

	^ self currentWorld primaryHand
]

{ #category : '*Morphic-Base' }
Object >> defaultLabel [
	"Answer the default label to be used in morphs."

	^ self class name
]

{ #category : '*Morphic-Base' }
Object >> dragPassengersFor: item inMorph: dragSource [

	^ { item }
]

{ #category : '*Morphic-Base' }
Object >> hasModelYellowButtonMenuItems [

	^ Morph cmdGesturesEnabled
]

{ #category : '*Morphic-Base' }
Object >> iconFormSetNamed: aSymbol [

	^ Smalltalk ui icons iconFormSetNamed: aSymbol
]

{ #category : '*Morphic-Base' }
Object >> iconNamed: aSymbol [

	^ Smalltalk ui icons iconNamed: aSymbol
]

{ #category : '*Morphic-Base' }
Object >> iconOrThumbnailOfSize: aNumberOrPoint [
	"Answer an appropiate form to represent the receiver"

	^ nil
]

{ #category : '*Morphic-Base' }
Object >> isTransferable [

	^ false
]

{ #category : '*Morphic-Base' }
Object >> okToClose [
	"Sent to models when a window closing.
	Allows this check to be independent of okToChange."

	^true
]

{ #category : '*Morphic-Base' }
Object >> systemIconName [

	^ #class
]

{ #category : '*Morphic-Base' }
Object >> taskbarIcon [
	"Answer the icon for the receiver in a task bar
	or nil for the default."

	self
		deprecated: 'The direct usage of #taskbarIcon is not recommended anymore since it
	introduces a dependency with UI layer we do not want to have here. Instead, our UI
	components should us #taskbarIconName to get the icon to use, and then user can resolve it
	as their want (since different applications can have different iconpacks, etc.) #Pharo11'
		transformWith: '`@receiver taskbarIcon' -> '`@receiver iconNamed: `@receiver taskbarIconName'.

	^ self iconNamed: self taskbarIconName
]

{ #category : '*Morphic-Base' }
Object class >> taskbarIconName [
	"Answer the icon for an instance of the receiver in a task bar"

	^#smallWindow
]

{ #category : '*Morphic-Base' }
Object >> taskbarIconName [
	"Answer the icon for the receiver in a task bar
	or nil for the default."

	^self class taskbarIconName
]

{ #category : '*Morphic-Base' }
Object class >> taskbarLabel [
	"Answer the label string for the receiver in a task bar
	or nil for the default."

	^nil
]

{ #category : '*Morphic-Base' }
Object >> taskbarLabel [
	"Answer the label string for the receiver in a task bar
	or nil for the default."

	^self class taskbarLabel
]

{ #category : '*Morphic-Base' }
Object >> transferFor: passenger from: aMorph [

	^ TransferMorph withPassenger: passenger from: aMorph
]

{ #category : '*Morphic-Base' }
Object >> treeNodeHead [

	^ self
]

{ #category : '*Morphic-Base' }
Object >> treeNodeTail [

	^ nil
]

{ #category : '*Morphic-Base' }
Object >> wantsVisualFeedback [

	^ true
]
