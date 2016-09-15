rendering
render
	| linkShape |
	self setPaint.
	
	linkShape := athensCanvas createPath: [:builder |		
		builder
			absolute;
			moveTo: checkpoints first;
			lineTo: checkpoints last.
		].

	athensCanvas paintMode restoreAfter: [
		athensCanvas paintMode difference.
		athensCanvas drawShape: linkShape.
	]