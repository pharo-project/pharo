testing
value: theValue start: tokenStart cause: errorCause location: errorPosition
	self value: theValue start: tokenStart .
	location := errorPosition.
	cause := errorCause .