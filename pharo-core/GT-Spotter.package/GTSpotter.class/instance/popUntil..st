actions api
popUntil: aStep

	[ self steps last = aStep ]
		whileFalse: [ self popStep ]