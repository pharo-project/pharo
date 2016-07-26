rendering
renderOn: canvas

	"
	set up clipping
	- draw line by line
	"
	| renderer |

	canvas pathTransform restoreAfter: [

		canvas pathTransform translateBy: offset negated.
			
		renderer := TxTextRenderer new.
		renderer renderLines: layout lines on: canvas.
	
	]