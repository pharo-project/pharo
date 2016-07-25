settings
lineNumbersBackgroundColor
	^ LineNumbersBackgroundColor ifNil: [ LineNumbersBackgroundColor := Smalltalk ui theme backgroundColor darker] 