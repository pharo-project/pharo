tests
testCreation
	"By now, only creation method is newFrom:"

	| cs1 wcs1 cs2 wcs2 byteString wideString |
	byteString := 'aeiouy'.
	wideString := 'aeiouy' copyWith: 340 asCharacter.

	cs1 := CharacterSet newFrom: byteString.
	wcs1 := WideCharacterSet newFrom: byteString.
	self assert: wcs1 equals: cs1.
	self assert: cs1 equals: wcs1.
	self assert: wcs1 size equals: byteString size.
	
	cs2 := CharacterSet newFrom: wideString.
	wcs2 := WideCharacterSet newFrom: wideString.
	self assert: wcs2 equals: cs2.
	self assert: cs2 equals: wcs2.
	self assert: wcs2 size = wideString size