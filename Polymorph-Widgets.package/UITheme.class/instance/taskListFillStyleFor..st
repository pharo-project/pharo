fill-styles
taskListFillStyleFor: aTasklist
	
	| aColor result |
	aColor := aTasklist color.
	result := GradientFillStyle ramp: {
			0.0->aColor whiter whiter. 0.2->aColor lighter.
			0.8->aColor darker. 1.0->aColor blacker}.
	^ result		
		origin: aTasklist topLeft;
		direction: 0 @ aTasklist height;
		radial: false;
		yourself.