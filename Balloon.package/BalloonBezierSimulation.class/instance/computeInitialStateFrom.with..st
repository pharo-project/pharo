computing
computeInitialStateFrom: source with: transformation
	"Compute the initial state in the receiver."
	start := (transformation localPointToGlobal: source start) asIntegerPoint.
	end := (transformation localPointToGlobal: source end) asIntegerPoint.
	via := (transformation localPointToGlobal: source via) asIntegerPoint.