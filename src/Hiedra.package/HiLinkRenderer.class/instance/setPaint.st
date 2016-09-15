accessing
setPaint
"	| paint |
	paint := athensCanvas surface 
		createLinearGradient: { 0 -> linkBuilder color muchLighter.  1 -> linkBuilder color } 
		start: checkpoints first
		stop: checkpoints last.
"
	(athensCanvas setStrokePaint: linkBuilder color) 
		width: linkBuilder width.
		
	athensCanvas paint 
		dashes: linkBuilder dashes 
		offset: 0