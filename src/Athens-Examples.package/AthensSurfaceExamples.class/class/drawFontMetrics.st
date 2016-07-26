examples
drawFontMetrics
"
self  drawFontMetrics
"
	|   font |
	
	
	font := LogicalFont familyName: 'Arial' pointSize: 10.
	self openViewOn: [ :can |

		can pathTransform restoreAfter: [

		can pathTransform translateX: 30 Y: 30;
		scaleBy: 20.

		can setPaint: (Color r:1 g:0.9 b:0.9);
			drawShape: (0@0 corner: 400@ (font getPreciseHeight)).		

		can setPaint: (Color r: 0.9 g: 0.9 b: 1);
			drawShape: (0@0 corner: 400@ (font getPreciseAscent+font getPreciseDescent)).		

		(can setStrokePaint: Color green) width: 0.05.
		can drawShape: (can createPath: [:builder |
			builder 
				moveTo: 0@0;
				lineTo: 0@font getPreciseAscent
			 ] ).

		(can setStrokePaint: Color red) width: 0.05.
		can drawShape: (can createPath: [:builder |
			builder 
				moveTo: 0@font getPreciseAscent;
				lineTo: 0@font getPreciseDescent
			 ] ).

		(can setStrokePaint: Color blue) width: 0.05.
		can drawShape: (can createPath: [:builder |
			builder 
				moveTo: 0@font getPreciseAscent;
				lineTo: 400@0
			 ] ).
				
		can setFont: font.
		can setPaint: (Color black).
		can pathTransform translateX: 0 Y: (font getPreciseAscent).
		can drawString: 'yh'.
		] ]

 