testing
testKnownEncodingIdentifiers
	| all minimal asciiString |
	all := ZnCharacterEncoder knownEncodingIdentifiers asSet.
	minimal := #(utf8 latin1 null ascii iso88591) asSet.
	"make sure at least a minimal set is present"
	self assert: (all intersection: minimal) equals: minimal.
	asciiString := String withAll: ($a to: $z) , ($A to: $Z) , ($0 to: $9).
	"make sure that each identifier can be used to instanciate a decoder,
	and that those decoders at least work on a ASCII string in both directions"
	all do: [ :each |
		| encoder bytes |
		encoder := ZnCharacterEncoder newForEncoding: each.
		bytes := encoder encodeString: asciiString.
		self assert: (encoder decodeBytes: bytes) equals: asciiString ]