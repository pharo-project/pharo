drawing
drawOnAthensCanvas: aCanvas
	"nothing"
	
"	aCanvas setPaint: (Color red alpha: 0.4).
	aCanvas pathTransform restoreAfter: [ 

		aCanvas drawShape: (bounds expandBy: 3) .
	].

"