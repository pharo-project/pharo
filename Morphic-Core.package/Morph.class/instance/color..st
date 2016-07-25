accessing
color: aColor
	"Set the receiver's color.  Directly set the color if appropriate, else go by way of fillStyle"

	(aColor isColor or: [aColor isKindOf: InfiniteForm]) ifFalse:[^ self fillStyle: aColor].
	color = aColor ifFalse:
		[self assureExtension.
		extension fillStyle: nil.
		color := aColor.
		self changed]