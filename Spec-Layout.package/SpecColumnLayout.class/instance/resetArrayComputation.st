private
resetArrayComputation

	super resetArrayComputation.
	
	size := self commands size.
	numberOfProportionals := 0.
	lastFractionIndex := 0.
	autoFractionComputation := true.
	currentOffset := 0.
	currentProportional := 0