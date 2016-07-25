as yet unclassified
targetPoint: aPoint
	"Set the reference point of the target."

	|minExt rect |
	rect := self target bounds withSideOrCorner: self edgeName setToPoint: aPoint.
	minExt := (self target layoutPolicy notNil and: [self target layoutPolicy isTableLayout])
		ifTrue: [self target layoutPolicy minExtentOf: self target in: self target layoutBounds]
		ifFalse: [self target minimumExtent].
	rect width <= minExt x ifTrue: [
		rect := self edgeName = #left
			ifTrue: [rect withSideOrCorner: #left setToPoint: self target bounds bottomRight - minExt]
			ifFalse: [rect withSideOrCorner: #right setToPoint: self target bounds topLeft + minExt]].
	rect height <= minExt y ifTrue: [
		rect := self edgeName = #top
			ifTrue: [rect withSideOrCorner: #top setToPoint: self target bounds bottomRight - minExt]
			ifFalse: [rect withSideOrCorner: #bottom setToPoint: self target bounds topLeft + minExt]].
	self setTargetBounds: rect