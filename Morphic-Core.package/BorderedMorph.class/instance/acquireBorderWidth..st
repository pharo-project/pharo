geometry
acquireBorderWidth: aBorderWidth
	"Gracefully acquire the new border width, keeping the interior area intact and not seeming to shift"

	| delta |
	(delta := aBorderWidth- self borderWidth) = 0 ifTrue: [^ self].
	self bounds: ((self bounds origin - (delta @ delta)) corner: (self bounds corner + (delta @ delta))).
	self borderWidth: aBorderWidth.
	self layoutChanged