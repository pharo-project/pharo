access
hasFocus
	"hasFocus is currently set by mouse enter/leave events.
	This inst var should probably be moved up to a higher superclass."

	^ hasFocus ifNil: [false]