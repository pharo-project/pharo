copying
copyOffset: aPoint clipRect: sourceClip
	"Make a copy of me offset by aPoint, and further clipped
	by sourceClip, a rectangle in the un-offset coordinates"
	^ self copyOrigin: aPoint + origin
		clipRect: ((sourceClip translateBy: origin) intersect: clipRect ifNone: [ 0@0 corner: 0@0 ])