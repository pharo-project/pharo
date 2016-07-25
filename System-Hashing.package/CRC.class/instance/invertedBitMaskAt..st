private-bit-manipulation
invertedBitMaskAt: anInteger
	anInteger < 1 ifTrue: [ ^ 16rFFFFFFFF ].
	^ 16rFFFFFFFF bitAt: anInteger put: 0