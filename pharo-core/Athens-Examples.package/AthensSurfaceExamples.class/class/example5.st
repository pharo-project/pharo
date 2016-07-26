examples
example5
" draw a hollow rectangle (frame) using lineTo/moveTo commands,
 with path, consisting of two contours: outer and inner one.

Note how #moveTo: command implicitly starts new contour
when inssued in the middle of command chain.
self example5
"

	| surf |
	
	surf := self newSurface: 100@100.
	
	surf drawDuring: [:can |
		surf clear.
		
		can setPaint: Color blue.
		
		can drawShape: (
			can createPath: [:path |
				path 
					absolute;
					lineTo: 50@0;
					lineTo: 50@50;
					lineTo: 0@50;
					moveTo: 10@10;
					lineTo: 10@40;
					lineTo: 40@40;
					lineTo: 40@10
			])
	
	].

	Display getCanvas drawImage: 	surf asForm  at: 0@0
