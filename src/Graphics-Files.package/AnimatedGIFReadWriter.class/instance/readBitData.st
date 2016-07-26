private-decoding
readBitData
	| form |
	form := super readBitData.
	form offset: offset.
	^form