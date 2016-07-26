accessing
margins: aRectangle
	margins := aRectangle withRight: (aRectangle right max: self cursorWidth ).
