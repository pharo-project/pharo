resources
readResourceFrom: aStream 
	"Store a resource representation of the receiver on aStream.
	Must be specific to the receiver so that no code is filed out."

	| bitsSize msb |
	(aStream next: 4) asString = self resourceTag 
		ifFalse: 
			[aStream position: aStream position - 4.
			^self readNativeResourceFrom: aStream].
	width := aStream nextNumber: 4.
	height := aStream nextNumber: 4.
	depth := aStream nextNumber: 4.
	bitsSize := aStream nextNumber: 4.
	bitsSize = 0 
		ifFalse: 
			[bits := aStream next: bitsSize.
			^self].
	msb := (aStream nextNumber: 4) = 1.
	bitsSize := aStream nextNumber: 4.
	bits := Bitmap new: self bitsSize.
	(Form 
		extent: width @ height
		depth: depth
		bits: (aStream next: bitsSize * 4)) displayOn: self.
	msb = EndianDetector isBigEndian 
		ifFalse: 
			[Bitmap 
				swapBytesIn: bits
				from: 1
				to: bits size]