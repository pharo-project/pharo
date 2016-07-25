rectangle functions
withSideOrCorner: side setToPoint: newPoint minExtent: minExtent limit: limit
	"Return a copy with side set to newPoint"
	side = #top ifTrue: [^ self withTop: (newPoint y min: corner y - minExtent y max: limit + minExtent y)].
	side = #bottom ifTrue: [^ self withBottom: (newPoint y min: limit - minExtent y max: origin y + minExtent y)].
	side = #left ifTrue: [^ self withLeft: (newPoint x min: corner x - minExtent x max: limit + minExtent x)].
	side = #right ifTrue: [^ self withRight: (newPoint x min: limit - minExtent x max: origin x + minExtent x)].
	side = #topLeft ifTrue: [^ (newPoint min: corner - minExtent) corner: self bottomRight].
	side = #bottomRight ifTrue: [^ self topLeft corner: (newPoint max: origin + minExtent)].
	side = #bottomLeft ifTrue: [^ self topRight rectangle: ((newPoint x min: corner x - minExtent x) @ (newPoint y max: origin y + minExtent y))].
	side = #topRight ifTrue: [^ self bottomLeft rectangle: ((newPoint x max: origin x + minExtent x) @ (newPoint y min: corner y - minExtent y))].