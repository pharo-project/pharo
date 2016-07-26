examples
example2
"
Draw a path on surface.

self example2.


"
	self openViewOn: [ :can |
		can setPaint: Color blue.
		
		can drawShape: (
			can createPath: [:path |
				path 
					absolute;
					lineTo: 50@0;
					lineTo: 50@50;
					lineTo: 0@100
			])
	
	].
 