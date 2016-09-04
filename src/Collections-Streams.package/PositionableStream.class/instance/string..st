accessing - data
string: aString
	"Store the given string on this (binary) stream. The string must contain 65535 or fewer characters."

	aString size > 16rFFFF ifTrue: [self error: 'string too long for this format'].
	self uint16: aString size.
	self nextPutAll: aString asByteArray.
