geometry
layoutChanged

	"A submorph could have moved, thus changing my localBounds. Invalidate the cache."
	localBounds := nil.

	^super layoutChanged