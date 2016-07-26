examples
example1

"
self  example1
"
	self openViewOn: [ :can |
		can pathTransform restoreAfter: [ 

			can pathTransform translateX: 30 Y: 30.
			can pathTransform rotateByDegrees: 35.
			can setPaint: (Color red).
			
			can setShape: (-20@ -20 corner: 20@ 20).
			2 timesRepeat: [ can draw. can setPaint: (Color green alpha:0.5)].
			].
	].
