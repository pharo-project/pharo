private
swapEndianness
	self isLittleEndian
		ifTrue: [ self beBigEndian ]
		ifFalse: [ self beLittleEndian ]