private
pathForBottomRightBorderOf: aRectangle on: anAthensCanvas
	^ anAthensCanvas
		createPath: [ :builder | 
			builder
				absolute;
				moveToX: aRectangle left Y: aRectangle bottom;
				lineToX: aRectangle right Y: aRectangle bottom;
				lineToX: aRectangle right Y: aRectangle top;
				lineToX: aRectangle right - width Y: aRectangle top + width;
				lineToX: aRectangle right - width Y: aRectangle bottom - width;
				lineToX: aRectangle left + width Y: aRectangle bottom - width;
				close ]