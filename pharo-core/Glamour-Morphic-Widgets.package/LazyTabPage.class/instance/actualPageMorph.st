accessing
actualPageMorph
	^ actualPageMorph ifNil: 
		[actualPageMorph := 
			self lazyPageMorphCreation value
				hResizing: #spaceFill;
				vResizing: #spaceFill;
				yourself]