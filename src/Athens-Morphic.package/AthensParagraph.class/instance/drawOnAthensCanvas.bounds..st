drawing
drawOnAthensCanvas: canvas bounds: aRectangle 

	| renderer |
	"self halt."
"	canvas
		clipBy: aRectangle 
		during: [  
"			renderer := AthensTextRenderer new.
			renderer render: text lines: lines on: canvas.
"		].
"