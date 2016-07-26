geometry
minExtent
	| minH |
	minH := scrollPane ifNil: [ 0 ] ifNotNil: [ 60 ].
	^ ((self textStyle defaultFont widthOfString: 'XXX') + self margins left + self margins right)
		@ (self textStyle lineGrid + self margins top + self margins bottom)