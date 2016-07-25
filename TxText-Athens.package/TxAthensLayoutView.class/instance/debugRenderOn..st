debug rendering
debugRenderOn: canvas

	"
	set up clipping
	- draw line by line
	"
	| renderer |

	canvas
		setStrokePaint: Color green;
		drawShape: (0@0 corner: self extent).

	renderer := TxTextRenderer new.
	renderer renderLines: layout lines on: canvas.
	
