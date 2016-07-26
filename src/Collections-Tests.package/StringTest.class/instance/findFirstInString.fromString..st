tests - indexof
findFirstInString: aString fromString: searchString

	^ String findFirstInString: aString inSet: (CharacterSet newFrom: searchString) byteArrayMap startingAt: 1