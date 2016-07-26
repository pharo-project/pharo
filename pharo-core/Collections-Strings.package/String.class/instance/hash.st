comparing
hash
	"#hash is implemented, because #= is implemented"
	"ar 4/10/2005: I had to change this to use ByteString hash as initial 
	hash in order to avoid having to rehash everything and yet compute
	the same hash for ByteString and WideString."
	^ self class stringHash: self initialHash: ByteString hash