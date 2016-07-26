accessing
direction: aPoint
	"Change the effective direction of any oriented fill styles."

	|delta|
	delta := aPoint - self direction.
	self fillStyles reverseDo: [:fs |
		fs isOrientedFill ifTrue: [
			fs direction: fs direction + delta]]