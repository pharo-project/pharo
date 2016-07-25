formatting
maxFormatterLineLength
	^ self textArea wrapped
		ifTrue: [ (self textArea innerBounds width / 9) integerPart ]
		ifFalse: [ 70 ]