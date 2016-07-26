accessing
previousCharacterIfAbsent: aBlock

	^ self text at: self startIndex - 1 ifAbsent: aBlock