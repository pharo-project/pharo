as yet unclassified
drawQuadFrom: pt1 via: pt2 to: pt3 on: canvas
	| curve triangle middleRay curveBlock |

	curveBlock := [ :builder | 
		builder 
			absolute;
			moveTo: pt1;
			curveVia: pt2 to: pt3
		 ].

	curve := canvas createPath: curveBlock.

	canvas setStrokePaint: self curveColor.
	canvas drawShape: curve.
	
	triangle :=  canvas createPath: [ :builder | 
		builder 
			absolute;
			moveTo: pt1;
			lineTo: pt2;
			lineTo: pt3;
			lineTo: pt1.
	].
	canvas setStrokePaint: self triangleColor.
	canvas drawShape: triangle.

	middleRay :=  canvas createPath: [ :builder | 
		builder 
			absolute;
			moveTo: pt2;
			lineTo: (pt1 + pt3)/2
	].

	canvas setStrokePaint: self middleRayColor.
	canvas drawShape: middleRay.

	triangle := AthensSimplePathBuilder createPath: curveBlock.
	
	triangle := canvas surface createPath: [ :builder | 
		AthensCurveFlattener new
			dest: builder;
		flattenPath: triangle transform: (canvas pathTransform).
	].
	
	canvas 
		setStrokePaint: Color yellow;
		drawShape: triangle.
	
