canvas drawing-ovals
fillOval: aRectangle fillStyle: aFillStyle borderWidth: aBorderWidth borderColor: aBorderColor
	self canvas setPaint: aFillStyle.
	self canvas pathTransform
		restoreAfter: [ 
			| path t |
			path := self createClosedEllipsePathIn: aRectangle.
			t := self canvas pathTransform.
			t translateBy: aRectangle origin.
			t scaleBy: 1 @ (aRectangle height / aRectangle width).
			self canvas setShape: path.
			self canvas draw.
			(self canvas setStrokePaint: aBorderColor) width: aBorderWidth.
			self canvas draw ]