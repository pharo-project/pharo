manage move
beginMove: touchId at: position
	moveToDraw at: touchId put: (SDL2TouchStateMachine for: position * self defaultExtent)