primitives
stringHash: aString initialHash: speciesHash
	| stringSize hash low |
	stringSize := aString size.
	hash := speciesHash bitAnd: 16rFFFFFFF.
	1 to: stringSize do: [:pos |
		hash := hash + (aString basicAt: pos).
		"Begin hashMultiply"
		low := hash bitAnd: 16383.
		hash := (16r260D * low + ((16r260D * (hash // 16384) + (16r0065 * low) bitAnd: 16383) * 16384)) bitAnd: 16r0FFFFFFF.
	].
	^ hash.
