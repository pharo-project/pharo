instance creation
topLeftColor: aColor1 bottomRightColor: aColor2 width: aNumber on: anAthensCanvas
	^ self new
		topLeftPaint: (aColor1 asAthensPaintOn: anAthensCanvas);
		bottomRightPaint: (aColor2 asAthensPaintOn: anAthensCanvas);
		width: aNumber;
		yourself