filein/out
replaceByResource: aForm
	"Replace the receiver by some resource that just got loaded"
	(self extent = aForm extent and:[self depth = aForm depth]) ifTrue:[
		bits := aForm bits.
	].