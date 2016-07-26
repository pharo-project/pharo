private
isLineTooLong: aString
	^ self currentLineLength + (aString indexOf: Character cr ifAbsent: [ aString size ])
		>= self maxLineLength