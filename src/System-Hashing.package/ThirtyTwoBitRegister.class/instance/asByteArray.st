converting
asByteArray
	^ ByteArray with: (low bitAnd: 16rFF) with: (low bitShift: -8) with: (hi bitAnd: 16rFF) with: (hi bitShift: -8)