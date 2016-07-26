fill-styles
dockingBarNormalFillStyleFor: aToolDockingBar
	"Return the normal docking bar fillStyle for the given color."
	
	|aColor|
	aColor := aToolDockingBar originalColor.
	^(GradientFillStyle ramp: {
			0.0->aColor whiter whiter. 0.2->aColor lighter.
			0.8->aColor darker. 1.0->aColor blacker})
		origin: aToolDockingBar topLeft;
		direction: (aToolDockingBar isVertical
			ifTrue: [aToolDockingBar width @ 0]
			ifFalse: [0 @ aToolDockingBar height]);
		radial: false