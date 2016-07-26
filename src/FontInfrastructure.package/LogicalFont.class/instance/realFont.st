accessing
realFont
	realFont ifNil:[ realFont := self findRealFont ].
	realFont validate.
	^ realFont