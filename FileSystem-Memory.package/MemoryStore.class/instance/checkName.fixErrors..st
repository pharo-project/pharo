public
checkName: aString fixErrors: fixErrors
	aString ifEmpty: [ self error: 'zero length file name' ].
	^ aString