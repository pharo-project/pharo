accessing
handlesEncoding: string
	"Return true when my instances handle the encoding described by string"
	
	^ (self canonicalEncodingIdentifier: string) = 'utf8'