layout
submorphBounds
	"Private. Compute the actual full bounds of the receiver, optimized for speed"
	self hasSubmorphs ifFalse: [^ nil].
	^ self firstSubmorph topLeft corner: owner scroller bounds bottomLeft + (0@ owner extraScrollRange)
