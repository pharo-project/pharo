geometry
closestSegmentTo: aPoint
	"Answer the starting index of my (big) segment nearest to aPoint"
	| closestPoint minDist vertexIndex closestVertexIndex |
	vertexIndex := 0.
	closestVertexIndex := 0.
	closestPoint := minDist := nil.
	self lineSegmentsDo:
		[:p1 :p2 | | curvePoint dist | 
		(p1 = (self vertices at: vertexIndex + 1))
			ifTrue: [ vertexIndex := vertexIndex + 1 ].
		curvePoint := aPoint nearestPointOnLineFrom: p1 to: p2.
		dist := curvePoint dist: aPoint.
		(closestPoint isNil or: [dist < minDist])
			ifTrue: [closestPoint := curvePoint.
					minDist := dist.
					closestVertexIndex := vertexIndex. ]].
	^ closestVertexIndex