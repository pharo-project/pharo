accessing
formattedCodeWithMaxLineLength: anInteger
	^ self formatterClass new
		containerWidth: anInteger;
		format: self