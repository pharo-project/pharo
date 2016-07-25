accessing
handlesEncoding: string
	"Return true when my instances handle the encoding described by string"

	^ #( 'utf32' 'utf32be' 'utf32le' 'ucs4' 'ucs4be' 'ucs4le') 
			includes: (self canonicalEncodingIdentifier: string)