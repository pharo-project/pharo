initialization
initialize
	super initialize.
	lineStart := 0.
	self indent: 0.
	lookaheadCode := IdentityDictionary new.
	codeStream := (String new: 256) writeStream.
	