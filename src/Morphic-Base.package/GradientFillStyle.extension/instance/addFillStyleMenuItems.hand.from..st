*Morphic-Base-Balloon
addFillStyleMenuItems: aMenu hand: aHand from: aMorph
	"Add the items for changing the current fill style of the receiver"
	self isRadialFill 
		ifTrue: [ aMenu add: 'linear gradient' target: self selector: #beLinearGradientIn: argument: aMorph ] 		ifFalse: [ aMenu add: 'radial gradient' target: self selector: #beRadialGradientIn: argument: aMorph ].
	aMenu addLine.
	aMenu add: 'change first color' target: self selector: #changeFirstColorIn:event: argument: aMorph.
	aMenu add: 'change second color' target: self selector: #changeSecondColorIn:event: argument: aMorph.
	aMenu addLine.
	super addFillStyleMenuItems: aMenu hand: aHand from: aMorph.