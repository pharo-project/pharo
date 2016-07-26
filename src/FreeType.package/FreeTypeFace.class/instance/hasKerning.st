testing
hasKerning
	^hasKerning ifNil:[
		[hasKerning := self primHasKerning = 64] 
			on: Error do:[:e | hasKerning := false].
		hasKerning]