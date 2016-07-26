testing
isEncrypted
	"Return true if this member is encrypted (this is unsupported)"
	^ (self bitFlag bitAnd: 1) ~= 0