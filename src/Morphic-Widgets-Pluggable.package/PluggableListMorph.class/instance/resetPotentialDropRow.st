drag and drop
resetPotentialDropRow
	potentialDropRow ifNotNil: [
	potentialDropRow ~= 0 ifTrue: [
		potentialDropRow := 0.
		self changed. ] ]