testing
testRecompiling

	| traitSrcPtr methodSrcPtr |
	
	traitSrcPtr := self t1 methods first sourcePointer.
	methodSrcPtr := self c1 methods first sourcePointer.
	
	self t1 recompile.
	self c1 recompile.
	
	self
		assert: traitSrcPtr equals: self t1 methods first sourcePointer;
		assert: methodSrcPtr equals: self c1 methods first sourcePointer
	