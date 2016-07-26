private
resetArrayComputation

	super resetArrayComputation.
	
	currentOffset := 0.
	currentProportional := 0.
	numberOfProportionals := 0.
	lastFractionIndex := 0.
	autoFractionComputation := true.
	size := self commands size