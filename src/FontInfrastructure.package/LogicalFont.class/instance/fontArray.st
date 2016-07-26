forwarded to realFont
fontArray
	| real | 
	real := self realFont.
	^real isFontSet 
		ifTrue: [real fontArray] 
		ifFalse: [{self}]