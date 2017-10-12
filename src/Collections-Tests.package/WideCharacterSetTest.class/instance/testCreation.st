tests
testCreation
	"By now, only creation method is newFrom:"

	| cs1 wcs1 cs2 wcs2 byteString wideString |
	byteString := 'aeiouy'.
	wideString := 'aeiouy' copyWith: 340 asCharacter.

	cs1 := CharacterSet newFrom: byteString.
	wcs1 := WideCharacterSet newFrom: byteString.
	self should: [wcs1 = cs1].
	self should: [wcs1 size = byteString "asSet" size].
	
	cs2 := CharacterSet newFrom: wideString.
	wcs2 := WideCharacterSet newFrom: wideString.
	self should: [wcs2 = cs2].
	self should: [wcs2 size = wideString "asSet" size].
	
	self should: [(byteString indexOfAnyOf: wcs1) = 1] description: 'This should used optimized byteArrayMap method'.
	self should: [(byteString indexOfAnyOf: wcs2) = 1] description: 'This should used optimized byteArrayMap method'.
	
	self should: [('bcd' indexOfAnyOf: wcs1) = 0] description: 'This should used optimized byteArrayMap method'.
	self should: [('bcd' indexOfAnyOf: wcs2) = 0] description: 'This should used optimized byteArrayMap method'.