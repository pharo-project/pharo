signaling
signal: anErrorMessage at: errorPosition
	^ (self new)
		position: errorPosition;
		signal: anErrorMessage