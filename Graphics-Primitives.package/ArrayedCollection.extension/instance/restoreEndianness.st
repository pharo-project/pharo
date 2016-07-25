*Graphics-Primitives
restoreEndianness
 	"This word object was just read in from a stream.  It was stored in Big Endian (Mac) format.  Reverse the byte order if the current machine is Little Endian.
 	We only intend this for non-pointer arrays.  Do nothing if I contain pointers."
 
 	self class isPointers | self class isWords not ifTrue: [^self].
 	EndianDetector isLittleEndian 
 		ifTrue: 
 			[Bitmap 
 				swapBytesIn: self
 				from: 1
 				to: self basicSize]