drawing
drawOn: aCanvas

	touchPoints := Dictionary new.
	aCanvas
		clipBy: self bounds
		during: [:clippedCanvas |
			clippedCanvas
				fillRectangle: self bounds
				color: Color white.

			self 
				drawMonthHeaderOn: clippedCanvas;
				drawWeekDayNamesOn: clippedCanvas;
				drawDaysOn: clippedCanvas;
				drawTodayOn: aCanvas.

			clippedCanvas
				frameRectangle: self bounds
				width: 1
				color: Color black].



	