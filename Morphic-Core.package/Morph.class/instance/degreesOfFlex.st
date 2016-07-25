rotate scale and flex
degreesOfFlex
	"Return any rotation due to flexing"
	"NOTE: because renderedMorph, which is used by the halo to set heading, goes down through dropShadows as well as transformations, we need this method (and its other implems) to come back up through such a chain."
	^ 0.0