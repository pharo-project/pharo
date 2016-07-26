figures
roundedRectanglePathOn: aCanvas

	^ aCanvas createPath: [:builder |
		builder
			absolute;
			moveTo: 70@100;
			lineTo: 330@100;
			cwArcTo: 350@120 angle: 90 degreesToRadians;
			lineTo: 350@280;
			cwArcTo: 330@300 angle: 90 degreesToRadians;
			lineTo: 70@300;
			cwArcTo: 50@280 angle: 90 degreesToRadians;
			lineTo: 50@120;
			cwArcTo: 70@100 angle: 90 degreesToRadians
	].

