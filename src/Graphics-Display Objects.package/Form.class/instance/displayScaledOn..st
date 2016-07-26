displaying
displayScaledOn: aForm
	"Display the receiver on aForm, scaling if necessary.
		Form fromUser displayScaledOn: Display.
	"
	self extent = aForm extent ifTrue:[^self displayOn: aForm].
	(WarpBlt current toForm: aForm)
		sourceForm: self destRect: aForm boundingBox;
		combinationRule: Form paint;
		cellSize: 2;
		warpBits.