instance creation
detectEncoding: bytes
	"Return one of my instances capable of decoding bytes.
	This is done by successively trying known encodings in a specific order.
	If no one is found, signal ZnCharacterEncodingError.
	This is a heuristic and unreliable [https://en.wikipedia.org/wiki/Charset_detection]."
	
	| candidates |
	"Set up an ordered candidates list, 7-bit ascii and utf8 are reasonably reliable, iso88591 is a reasonable default"
	candidates := #(ascii utf8 iso88591).
	candidates := candidates , (ZnByteEncoder knownEncodingIdentifiers difference: candidates).
	candidates := candidates , (self knownEncodingIdentifiers difference: candidates).
	"Try each and return the first one that succeeeds."
	candidates do: [ :identifier | | encoder |
		encoder := self newForEncoding: identifier.
		[ ^ encoder decodeBytes: bytes; yourself ] on: ZnCharacterEncodingError do: [ ] ].
	ZnCharacterEncodingError signal: 'No suitable encoder found'