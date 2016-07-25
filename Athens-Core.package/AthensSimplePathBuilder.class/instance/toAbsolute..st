private
toAbsolute: aPoint
	| pt |
	pt := absolute 
		ifTrue: [ aPoint ]
		ifFalse: [ lastSegment endPoint + aPoint ].

	"note the coordinate to calculate the path's bounding box"
	xMin ifNil: [
		xMin := xMax := pt x.
		yMin := yMax := pt y.	
		] 
	ifNotNil: [
		xMin := pt x min: xMin.
		yMin := pt y min: yMin.
		xMax := pt x max: xMax.
		yMax := pt y max: yMax.	
	].
	^ pt