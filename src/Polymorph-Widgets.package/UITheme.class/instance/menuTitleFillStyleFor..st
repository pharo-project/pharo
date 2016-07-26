fill-styles
menuTitleFillStyleFor: aMenuTitle
	"Return the fillStyle for the given title menu."
	| fill |

	fill := GradientFillStyle ramp: {0.0 -> aMenuTitle color twiceLighter. 1 -> aMenuTitle color twiceDarker}.
	fill
		origin: aMenuTitle topLeft;
		direction: aMenuTitle width @ 0.
	^ fill