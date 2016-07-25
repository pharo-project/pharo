accessing
canonicalEncodingIdentifier: string
	^ (string select: [ :each | each isAlphaNumeric ]) asLowercase