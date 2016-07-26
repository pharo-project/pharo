converting
collectPixels: aBlock
	"Create a new copy of the receiver with all the pixels transformed by aBlock"
	self depth = 32 ifFalse:[
		"Perform the operation in 32bpp"
		^((self asFormOfDepth: 32) collectPixels: aBlock) asFormOfDepth: self depth].
	self unhibernate. "ensure unhibernated before touching bits"
	^Form 
		extent: self extent 
		depth: self depth
		bits: (self bits collect: aBlock)