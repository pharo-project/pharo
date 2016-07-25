as yet unclassified
setTargetBounds: aRect
	"Set the target bounds, taking owner into account if required."

	self target bounds: aRect.
	self fitTargetOwner ifTrue: [
		self fitTargetBoundsInOwner: aRect]