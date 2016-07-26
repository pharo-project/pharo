*Athens-Morphic
drawLabelOnAthensCanvas: anAthensCanvas
	anAthensCanvas pathTransform translateX: 0 Y: self fontToUse getPreciseAscent.
			anAthensCanvas
				setPaint: self color;
				setFont: self fontToUse;
				drawString: contents.