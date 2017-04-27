building
athensSurface
	| athensSurface ruler |
	ruler := HiRuler new
		values: values;
		linkBuilders: linkBuilders;
		build;
		yourself.

	"FIX: it's (ruler numberOfColumns + 1) because of the left margin"
	athensSurface := AthensCairoSurface extent: 
		((ruler numberOfColumns + 1) * self columnSize) @ (self numberOfRows * self rowSize).
		
	athensSurface drawDuring: [ :athensCanvas | 

		"apply left margin"
		athensCanvas pathTransform
			translateX: self columnSize
			Y: 0.
					
		(HiRulerRenderer 
			model: self 
			athensCanvas: athensCanvas
			ruler: ruler) 
			render ].

	^ athensSurface