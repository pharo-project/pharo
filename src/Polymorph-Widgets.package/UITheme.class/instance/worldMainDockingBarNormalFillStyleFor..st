fill-styles
worldMainDockingBarNormalFillStyleFor: aDockingBar
	"Return the world main docking bar fillStyle for the given docking bar."
	
	|aColor|
	aColor := aDockingBar originalColor.
	^(GradientFillStyle ramp: {0.0 -> aColor muchLighter. 1.0 -> aColor})
		origin: aDockingBar topLeft;
		direction: (aDockingBar isVertical
			ifTrue: [aDockingBar width @ 0]
			ifFalse: [0 @ aDockingBar height]);
		radial: false