*Fuel
cleanCopy
	"Answer a copy of myself preserving in outerContext just the receiver and method, which are the only needed by a clean block closure."

	^ self shallowCopy
		cleanOuterContext;
		yourself