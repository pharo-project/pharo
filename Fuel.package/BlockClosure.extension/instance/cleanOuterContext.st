*Fuel
cleanOuterContext
	"Clean my outerContext preserving just the receiver and method, which are the only needed by a clean block closure."

	outerContext := outerContext cleanCopy