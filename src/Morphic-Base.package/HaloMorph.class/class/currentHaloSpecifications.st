halo theme
currentHaloSpecifications
	^ CurrentHaloSpecifications ifNil: [self installHaloTheme: #iconicHaloSpecifications]
