manage move
move: touchId at: position
	(moveToDraw at: touchId) add: position * self defaultExtent