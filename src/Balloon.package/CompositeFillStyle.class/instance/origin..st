accessing
origin: aPoint
	"Change the effective origin of any oriented fill styles."

	|delta|
	delta := aPoint - self origin.
	self fillStyles reverseDo: [:fs |
		fs isOrientedFill ifTrue: [
			fs origin: fs origin + delta]]
	