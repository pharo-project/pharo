objects from disk
restoreEndianness
	"This word object was just read in from a stream.  It was stored in Big Endian (Mac) format.  Swap each pair of bytes (16-bit word), if the current machine is Little Endian.
	Why is this the right thing to do?  We are using memory as a byteStream.  High and low bytes are reversed in each 16-bit word, but the stream of words ascends through memory.  Different from a Bitmap."
	| w b1 b2 b3 b4 |
	EndianDetector isLittleEndian ifTrue: 
		[ 1 
			to: self basicSize
			do: 
				[ :i | 
				w := self basicAt: i.
				b1 := w digitAt: 1.
				b2 := w digitAt: 2.
				b3 := w digitAt: 3.
				b4 := w digitAt: 4.
				w := (b1 << 24) + (b2 << 16) + (b3 << 8) + b4.
				self 
					basicAt: i
					put: w ] ]