testing
testUTF8ByteOrderMark
	"The Unicode Byte Order Mark (BOM) should be skipped."
	
	| bom string bytes encoder |
	encoder := ZnUTF8Encoder new.
	bom := #(239 187 191).
	string := 'élève en Français'.
	bytes := encoder encodeString: string.
	self assert: (encoder decodeBytes: bom, bytes) equals: string.
	self should: [ encoder decodeBytes: bom ] raise: ZnInvalidUTF8 