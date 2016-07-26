instance creation
value: value start: tokenStart cause: errorCause location: thePosition
	^ self new
		value: value
			start: tokenStart
			cause: errorCause
			location: thePosition;
		yourself