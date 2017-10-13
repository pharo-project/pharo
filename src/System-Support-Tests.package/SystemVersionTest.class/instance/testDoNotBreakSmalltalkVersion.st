tests - compatibility
testDoNotBreakSmalltalkVersion
	"TODO: This one might be yellow when we still set the suffix with build nr and hash from the outside."
	
	self checkIsCompatibleWithResultOfVersionMessage: Smalltalk version 
	