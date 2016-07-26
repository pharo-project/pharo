drawing
framePolyline: vertices on: aCanvas 
	"Frame the given rectangle on aCanvas"

	| prev next |
	prev := vertices first.
	2 to: vertices size
		do: 
			[:i | 
			next := vertices at: i.
			self 
				drawLineFrom: prev
				to: next
				on: aCanvas.
			prev := next]