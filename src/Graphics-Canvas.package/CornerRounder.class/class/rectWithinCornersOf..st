all
rectWithinCornersOf: aRectangle
	"Return a single sub-rectangle that lies entirely inside corners
	that are made by me.
	Used to identify large regions of window that do not need to be redrawn."

	^ aRectangle insetBy: 0@6