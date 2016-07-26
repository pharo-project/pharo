halo theme
allHaloSpecsFromArray: anArray
	^ anArray collect: [:quin | self haloSpecFromArray: quin]