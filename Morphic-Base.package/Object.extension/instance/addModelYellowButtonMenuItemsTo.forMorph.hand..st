*Morphic-Base
addModelYellowButtonMenuItemsTo: aCustomMenu forMorph: aMorph hand: aHandMorph 
	"The receiver serves as the model for aMorph; a menu is being constructed for the morph, and here the receiver is able to add its own items"
	Morph cmdGesturesEnabled ifTrue: [ 
		aCustomMenu add: 'inspect model'  target: self selector: #inspect.
	].
	^aCustomMenu
