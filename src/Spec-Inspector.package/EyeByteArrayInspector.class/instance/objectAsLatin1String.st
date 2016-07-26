converting
objectAsLatin1String
	| width str |
	str := String new:
		(width := self object size min: 5000).
	str
		replaceFrom: 1
		to: str size
		with: (self object copyFrom: 1 to: width).
	^ str