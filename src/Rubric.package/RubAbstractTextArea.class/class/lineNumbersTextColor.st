settings
lineNumbersTextColor
	^ LineNumbersTextColor ifNil: [ LineNumbersTextColor := Smalltalk ui theme lineNumberColor ]