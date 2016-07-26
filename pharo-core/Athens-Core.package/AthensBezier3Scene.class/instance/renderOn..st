as yet unclassified
renderOn: canvas

	| path pathBlock polygonizedPath |
	
	canvas surface clear:  Color black.
	pathBlock :=  [ :builder |
		builder 
			absolute;
			moveTo: pt1;
			curveVia: pt2 and: pt3 to: pt4
	 ].
	

	"First , we draw the curve using Cairo"	
	(canvas setStrokePaint: Color green).
	canvas drawShape: (canvas createPath: pathBlock).
		
	"draw a polygon, connecting control points"
	(canvas setStrokePaint: Color blue) width: 0.5.
	canvas drawShape: (canvas createPath: [ :builder |
		builder 
			absolute;
			moveTo: pt1;
			lineTo: pt2;
			lineTo: pt3;
			lineTo: pt4
		 ]).
	
		
	
	canvas setPaint: Color red.
	
	path := AthensSimplePathBuilder createPath: pathBlock.
"	polygonizedPath := path asPolygon: canvas pathTransform. "
	
	canvas drawShape: (
		canvas createPath: [ :builder | | flattener |
			builder absolute.
	
		flattener := AthensCurveFlattener new dest: builder.
		flattener transform: (canvas pathTransform). 
		flattener convert:path. 
	]).