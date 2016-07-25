measuring
widthOfString: aString
	aString ifNil: [^0].
	^self widthOfString: aString from: 1 to: aString size.
"
	TextStyle default defaultFont widthOfString: 'zort' 21
"