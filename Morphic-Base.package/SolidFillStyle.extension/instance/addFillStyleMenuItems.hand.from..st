*Morphic-Base
addFillStyleMenuItems: aMenu hand: aHand from: aMorph
	"Add the items for changing the current fill style of the receiver"
	aMenu add: 'change color...' target: self selector: #changeColorIn:event: argument: aMorph