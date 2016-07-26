computing
computeInitialStateFrom: source with: aTransformation
	"Compute the initial state in the receiver."
	start := (aTransformation localPointToGlobal: source start) asIntegerPoint.
	end := (aTransformation localPointToGlobal: source end) asIntegerPoint.