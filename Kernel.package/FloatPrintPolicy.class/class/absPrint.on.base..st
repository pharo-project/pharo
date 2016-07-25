printing
absPrint: float on: stream base: base
	"I delegate Float printing to the current dynamic value of myself"

	self value absPrint: float on: stream base: base