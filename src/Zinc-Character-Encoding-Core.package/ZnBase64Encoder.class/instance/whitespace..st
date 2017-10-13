initialization
whitespace: mode
	"Set the whitespace mode: 
	nil is no whitespace allowed, 
	#separator is CR, LF, FF, SPACE, TAB allowed,
	#any is all non-alphabet characters allowed (the default)"
	
	self assert: (#(nil separator any) includes: mode).
	whitespace := mode