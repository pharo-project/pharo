fill-styles-buttons
menuItemSelectedFillStyleFor: aMenuItem
	"Answer the selected fill style to use for the given menu item."
	
	| fill baseColor preferenced |
	Display depth <= 2 ifTrue: [^ Color gray].
	preferenced := self settings menuSelectionColor.
	baseColor := preferenced isNil
		ifTrue: [aMenuItem owner color negated]
		ifFalse: [preferenced].
	self preferGradientFill
		ifFalse: [^baseColor].
	fill := GradientFillStyle ramp: {0.0 -> baseColor twiceLighter . 1 -> baseColor twiceDarker}.
	fill
		origin: aMenuItem topLeft;
		direction: 0@aMenuItem height.
	^ fill