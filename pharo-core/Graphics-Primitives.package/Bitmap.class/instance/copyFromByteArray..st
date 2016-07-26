accessing
copyFromByteArray: byteArray 
	"This method should work with either byte orderings"

	| myHack byteHack |
	myHack := Form new hackBits: self.
	byteHack := Form new hackBits: byteArray.
	EndianDetector isLittleEndian ifTrue: [byteHack swapEndianness].
	byteHack displayOn: myHack