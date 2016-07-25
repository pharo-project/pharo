accessing
handlesEncoding: string
	"Return true when my instances handle the encoding described by string"

	^ #( 'utf16' 'utf16be' 'utf16le' ) includes: (self canonicalEncodingIdentifier: string)