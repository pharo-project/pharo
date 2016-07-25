fill-styles
worldMainDockingBarNormalFillStyleFor: aDockingBar
	"Return the world main docking bar fillStyle for the given docking bar."
	
	|aColor|
	aColor := aDockingBar originalColor alpha: 0.7.
	^(GradientFillStyle ramp: {
			0.0->(aColor alphaMixed: 0.3 with: (Color white alpha: aColor alpha)).
			0.8->aColor darker.
			1.0->aColor darker duller})
		origin: aDockingBar topLeft;
		direction: (aDockingBar isVertical
			ifTrue: [aDockingBar width @ 0]
			ifFalse: [0 @ aDockingBar height]);
		radial: false