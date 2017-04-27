primitives
stringHash: aString initialHash: speciesHash

	| stringSize hash low |
	<primitive: 'primitiveStringHash' module: 'MiscPrimitivePlugin'>

	<var: #aHash declareC: 'int speciesHash'>
	<var: #aString declareC: 'unsigned char *aString'>

	stringSize := aString size.
	hash := speciesHash bitAnd: 16rFFFFFFF.
	1 to: stringSize do: [:pos |
		hash := hash + (aString basicAt: pos).
		"Begin hashMultiply"
		low := hash bitAnd: 16383.
		hash := (16r260D * low + ((16r260D * (hash bitShift: -14) + (16r0065 * low) bitAnd: 16383) * 16384)) bitAnd: 16r0FFFFFFF.
	].
	^ hash