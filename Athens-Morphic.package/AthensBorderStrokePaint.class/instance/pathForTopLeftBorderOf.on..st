private
pathForTopLeftBorderOf: aRectangle on: anAthensCanvas
	^ anAthensCanvas
		createPath: [ :builder | 
			builder
				absolute;
				moveToX: aRectangle left Y: aRectangle bottom;
				lineToX: aRectangle left Y: aRectangle top;
				lineToX: aRectangle right Y: aRectangle top;
				lineToX: aRectangle right - width Y: aRectangle top + width;
				lineToX: aRectangle left + width Y: aRectangle top + width;
				lineToX: aRectangle left + width Y: aRectangle bottom - width;
				close ]