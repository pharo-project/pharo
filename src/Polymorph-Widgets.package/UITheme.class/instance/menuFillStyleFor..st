fill-styles
menuFillStyleFor: aMenu
	"Return the fillStyle for the given menu."
	
	| bc fill |
	bc := aMenu valueOfProperty: #basicColor ifAbsent: [self settings derivedMenuColor].
	fill := GradientFillStyle ramp: {0.0 -> (bc alphaMixed: 0.2 with: Color white). 1.0 -> bc}.
	fill
		radial: false;
		origin: aMenu topLeft;
		direction: 0 @ aMenu height.
	^ fill.