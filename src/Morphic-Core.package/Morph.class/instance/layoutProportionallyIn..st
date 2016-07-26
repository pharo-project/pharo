layout
layoutProportionallyIn: newBounds
	"Layout specific. Apply the given bounds to the receiver."
	| box frame |
	frame := self layoutFrame ifNil:[^self].
	"before applying the proportional values make sure the receiver's layout is computed"
	"compute the cell size the receiver has given its layout frame"
	box := frame layout: self bounds in: newBounds.
	(box = self bounds) ifTrue:[^self]. "no change"
	^self layoutInBounds: box.