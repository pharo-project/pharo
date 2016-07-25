accessing
window
	self announce: (GLMWindowRequest new
		action: [ :aWindow | ^ aWindow ]).
	^ nil