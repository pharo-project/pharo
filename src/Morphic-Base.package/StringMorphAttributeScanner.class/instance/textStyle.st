accessing
textStyle
	^self actualFont textStyle ifNil: [ TextStyle default ]