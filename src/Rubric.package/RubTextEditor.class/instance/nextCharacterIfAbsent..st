accessing
nextCharacterIfAbsent: aBlock

	^ self text at: self startIndex ifAbsent: aBlock