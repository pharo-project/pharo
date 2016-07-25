instance creation
bitPokerToForm: destForm 
	"Answer an instance to be used for valueAt: aPoint put: pixValue.
	The source for a 1x1 copyBits will be the low order of (bits at: 1)"
	| pixPerWord |
	pixPerWord := 32 // destForm depth.
	destForm unhibernate.
	^ self 
		destForm: destForm
		sourceForm: (Form 
				extent: pixPerWord @ 1
				depth: destForm depth)
		halftoneForm: nil
		combinationRule: Form over
		destOrigin: 0 @ 0
		sourceOrigin: (pixPerWord - 1) @ 0
		extent: 1 @ 1
		clipRect: (0 @ 0 extent: destForm extent)