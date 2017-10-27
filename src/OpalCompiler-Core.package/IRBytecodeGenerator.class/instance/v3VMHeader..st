results
v3VMHeader: literalsSize
	| header primBits endPrimNumber |
	self encoderClass name = #EncoderForSistaV1 ifTrue: [ 
		self error: 'SistaV1 non supported in non spur VM'].
	endPrimNumber := self endPrimNumber.
	primBits := endPrimNumber <= 16r1FF
		ifTrue: [endPrimNumber]
		ifFalse: ["For now the high bit of primitive no. is in the 29th bit of header"
				(endPrimNumber bitAnd: 16r1FF) + ((endPrimNumber bitAnd: 16r200) bitShift: 19)].
	header := (self numArgs bitShift: 24) +
				(self numTemps bitShift: 18) +
				(literalsSize bitShift: 9) +
				primBits.
	^ header