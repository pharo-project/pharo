event handling
mouseDown: event
	"Handle mouse down and mouse movement. Highlight the day under the mouse."

	| morphRelativeHitPoint |
	morphRelativeHitPoint := event cursorPoint translateBy: bounds origin negated.
	days do: [:each |
		each highlighted: (each bounds containsPoint: morphRelativeHitPoint)].
	self changed.
