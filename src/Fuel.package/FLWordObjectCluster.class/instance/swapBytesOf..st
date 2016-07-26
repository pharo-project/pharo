endianness
swapBytesOf: aWordObject
	Bitmap
                swapBytesIn: aWordObject
                from: 1
                to: aWordObject basicSize.
	^ aWordObject