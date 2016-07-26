geometry
layoutPosition
	"Answer the position that the cached form should be drawn
	based on the layout"
	
	self layout == #topCenter ifTrue: [^self innerBounds topCenter - (self cachedForm width // 2 @ 0)].
	self layout == #topRight ifTrue: [^self innerBounds topRight - (self cachedForm width @ 0)].
	self layout == #rightCenter ifTrue: [^self innerBounds rightCenter - (self cachedForm width @ (self cachedForm height // 2))].
	self layout == #bottomRight ifTrue: [^self innerBounds bottomRight - self cachedForm extent].
	self layout == #bottomCenter ifTrue: [^self innerBounds bottomCenter - (self cachedForm width // 2 @ self cachedForm height)].
	self layout == #bottomLeft ifTrue: [^self innerBounds bottomLeft - (0 @ self cachedForm height)].
	self layout == #leftCenter ifTrue: [^self innerBounds leftCenter - (0 @ (self cachedForm height // 2))].
	(self layout == #center or: [self layout == #scaledAspect]) ifTrue: [^self innerBounds center - (self cachedForm extent // 2)].
	^self innerBounds topLeft