layout
layoutProportionallyIn: newBounds
	"Layout specific. Apply the given bounds to the receiver."
	self layoutFrame ifNil:[^self].
	"before applying the proportional values make sure the receiver's layout is computed"
	self isAutoFit ifTrue: [self fullBounds].
	^super layoutProportionallyIn: newBounds 