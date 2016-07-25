as yet unclassified
selectColorAt: aPoint
	"Set the color at the given position."
	
	|b p|
	b := self innerBounds.
	p := (b containsPoint: aPoint)
		ifTrue: [aPoint]
		ifFalse: [b pointNearestTo: aPoint].
	p := p - b topLeft / b extent.
	self selectedColor: (Color
		h: self color hue
		s: p x
		v: 1.0 - p y)