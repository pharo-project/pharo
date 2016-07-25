accessing
snippets 
	^ snippets ifNil: [ snippets := IdentityDictionary new ]