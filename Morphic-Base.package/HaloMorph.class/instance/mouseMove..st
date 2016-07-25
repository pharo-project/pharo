event handling
mouseMove: evt
	"Drag our target around"
	| thePoint |
	thePoint := target point: (evt position - positionOffset) from: owner.
	target setConstrainedPosition: thePoint hangOut: true.