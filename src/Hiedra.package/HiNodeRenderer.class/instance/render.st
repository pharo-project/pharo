rendering
render
	"Render this node in the athensCanvas"

	self initializeShape.

	athensCanvas paintMode restoreAfter: [
		athensCanvas pathTransform restoreAfter: [ 
			athensCanvas pathTransform
				translateX: location x
				Y: location y.

			athensCanvas pathTransform restoreAfter: [ 
				athensCanvas pathTransform scaleBy: nodeBuilder width.
				athensCanvas setPaint: nodeBuilder color.
				athensCanvas drawShape: nodeShape ].
			
			athensCanvas pathTransform scaleBy: nodeBuilder width * 0.65.
			athensCanvas paintMode clear.
			athensCanvas drawShape: nodeShape ]]