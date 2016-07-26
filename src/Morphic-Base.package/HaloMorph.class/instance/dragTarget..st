events
dragTarget: event
	"Begin dragging the target"
	| thePoint |
	thePoint := target point: event position - positionOffset from: owner.
	target setConstrainedPosition: thePoint hangOut: true.
	event hand newMouseFocus: self.