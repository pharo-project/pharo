protocol
updateFillStyle
	"Update the fill style directions."

	|b fs|
	fs := self fillStyle.
	fs isOrientedFill ifTrue: [
		b := self innerBounds.
		fs origin: b topLeft.
		fs direction: (b isWide
			ifTrue: [b width@0]
			ifFalse: [0@b height])]