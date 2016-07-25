An Example how Slots can be useful for Morphic.

A Slot for a morph Ivar where the use case is to hold a reference to some morph, and changing that Ivar follows the pattern of:

setIvar: aMorph
	 ivar ifNotNil: [ ivar delete ].
	 ivar := aMorph
