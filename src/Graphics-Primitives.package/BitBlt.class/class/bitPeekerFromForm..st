instance creation
bitPeekerFromForm: sourceForm 
	"Answer an instance to be used extract individual pixels from the given Form. The destination for a 1x1 copyBits will be the low order bits of (bits at: 1)."
	| pixPerWord |
	pixPerWord := 32 // sourceForm depth.
	sourceForm unhibernate.
	^ self 
		destForm: (Form 
				extent: pixPerWord @ 1
				depth: sourceForm depth)
		sourceForm: sourceForm
		halftoneForm: nil
		combinationRule: Form over
		destOrigin: (pixPerWord - 1) @ 0
		sourceOrigin: 0 @ 0
		extent: 1 @ 1
		clipRect: (0 @ 0 extent: pixPerWord @ 1)