accessing
targetPoint: aPoint
	"Set the reference point of the target."

	|minExt rect|
	rect := self target bounds withSideOrCorner: self ptName setToPoint: aPoint.
	minExt := self target minimumExtent.
	rect width <= minExt x ifTrue: [
		(self ptName = #topLeft or: [self ptName = #bottomLeft])
			ifTrue: [rect := rect withSideOrCorner: #left setToPoint: self target bounds bottomRight - minExt]
			ifFalse: [rect := rect withSideOrCorner: #right setToPoint: self target bounds topLeft + minExt]].
	rect height <= minExt y ifTrue: [
		(self ptName = #topLeft or: [self ptName = #topRight])
			ifTrue: [rect := rect withSideOrCorner: #top setToPoint: self target bounds bottomRight - minExt]
			ifFalse: [rect := rect withSideOrCorner: #bottom setToPoint: self target bounds topLeft + minExt]].
	self target bounds: rect