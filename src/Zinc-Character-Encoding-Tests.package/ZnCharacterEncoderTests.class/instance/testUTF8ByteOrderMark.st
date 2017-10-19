testing
testUTF8ByteOrderMark
	"The Unicode Byte Order Mark (BOM) should be skipped."
	
	| bom string bytes encoder decodedString |
	encoder := ZnUTF8Encoder new.
	string := 'élève en Français'.
	bytes := encoder encodeStringWithByteOrderMark: string.
	self assert: (encoder decodeBytes: bytes) equals: string.
	
	string := 'Foo'.
	bytes := encoder encodeStringWithByteOrderMark: string.
	decodedString := String new: string size.
	ZnUTF8Encoder new 
		readInto: decodedString startingAt: 1 count: string size fromStream: bytes readStream. 
	self assert: decodedString equals: string.
	
	bom := encoder encodeStringWithByteOrderMark: ''.
	self should: [ encoder decodeBytes: bom ] raise: ZnInvalidUTF8