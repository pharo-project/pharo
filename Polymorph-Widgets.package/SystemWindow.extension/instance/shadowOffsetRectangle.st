*Polymorph-Widgets
shadowOffsetRectangle
	"Answer a rectangle describing the offsets to the
	receiver's bounds for a drop shadow."

	^self isActive
		ifTrue: [self theme windowActiveDropShadowOffsetRectangleFor: self]
		ifFalse: [self theme windowInactiveDropShadowOffsetRectangleFor: self]