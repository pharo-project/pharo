private
getBestLocation: vertices for: morph corner: cornerName
	| rect maxArea verts rectCorner morphPoint mbc a mp dir bestVerts result usableArea |
	"Choose rect independantly of vertice order or size. Would be nice it this took into account curveBounds but it does not." 
	rect := Rectangle encompassing: vertices.  
	maxArea := -1.
	verts := vertices.
	usableArea := (morph world ifNil: [self currentWorld]) viewBox.
	1 to: 4 do: [:i |
		dir := #(vertical horizontal) atWrap: i.
		verts := verts collect: [:p | p flipBy: dir centerAt: rect center].
		rectCorner := #(bottomLeft bottomRight topRight topLeft) at: i.
		morphPoint := #(topCenter topCenter bottomCenter bottomCenter) at: i.
		a := ((rect
			align: (rect perform: rectCorner)
			with: (mbc := morph boundsForBalloon perform: morphPoint))
				intersect: usableArea) area.
		(a > maxArea or: [a = rect area and: [rectCorner = cornerName]]) ifTrue:
			[maxArea := a.
			bestVerts := verts.
			mp := mbc]].
	result := bestVerts collect: [:p | p + (mp - bestVerts first)] "Inlined align:with:".
	^ result