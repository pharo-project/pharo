geometry
imageRectangleFromDisplayRectangle: aRectangle
	"Answer a rectangle in (original) image coordinates that
	corresponds to the given rectangle (in relative display coordinates)."

	|db|
	db := self displayBounds .
	db area = 0 ifTrue: [^db].
	self layout == #scaledAspect ifTrue: [
		^((aRectangle translateBy: self layoutPosition negated)
			scaleBy: self form width / db width) rounded].
	self layout == #scaled ifTrue: [
		^((aRectangle translateBy: self layoutPosition negated)
			scaleBy: (self form width / db width) @ (self form height / db height)) rounded].
	^self scale = 1
		ifTrue: [aRectangle translateBy: self layoutPosition negated]
		ifFalse: [((aRectangle translateBy: self layoutPosition negated)
					scaleBy: 1 / self scale) rounded]