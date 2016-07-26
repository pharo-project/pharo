*Athens-Morphic
asAthensPaintOn: canvas
	^ (canvas surface createFormPaint: self asForm)
		repeat;
		origin: self origin;
		direction: self direction;
		extent: self extent;
		yourself