initialization
initialize
	super initialize.
	lineStart := 0.
	self indent: 0.
	self isInCascadeNode: false.
	lookaheadCode := IdentityDictionary new.
	codeStream := WriteStream on: (String new: 256).
	context := DefaultPrettyPrintContext