testing
testByteStringSearch
	"By now, only creation method is newFrom:"

	| wcs1 wcs2 byteString wideString |
	byteString := 'aeiouy'.
	wideString := 'aeiouy' copyWith: 340 asCharacter.

	wcs1 := WideCharacterSet newFrom: byteString.
	wcs2 := WideCharacterSet newFrom: wideString.

	self should: [(byteString indexOfAnyOf: wcs1) = 1] description: 'This should used optimized byteArrayMap method'.
	self should: [(byteString indexOfAnyOf: wcs2) = 1] description: 'This should used optimized byteArrayMap method'.
	
	self should: [('bcd' indexOfAnyOf: wcs1) = 0] description: 'This should used optimized byteArrayMap method'.
	self should: [('bcd' indexOfAnyOf: wcs2) = 0] description: 'This should used optimized byteArrayMap method'.