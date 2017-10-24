fill-styles-buttons
menuItemInDockingBarSelectedFillStyleFor: aMenuItem
	"Answer the selected fill style to use for the given menu item that is in a docking bar."
	
	| fill baseColor |
	Display depth <= 2
		ifTrue: [^ Color gray].
	baseColor := self settings derivedMenuColor.
	self preferGradientFill ifFalse: [^baseColor].
	fill := GradientFillStyle ramp: {0.0 -> baseColor twiceDarker . 1 -> baseColor twiceLighter}.
	fill
		origin: aMenuItem topLeft;
		direction: 0@aMenuItem height.
	^ fill