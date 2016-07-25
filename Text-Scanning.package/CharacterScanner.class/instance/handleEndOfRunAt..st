stop conditions
handleEndOfRunAt: stopIndex
	" make sure the lastIndex is set to stopIndex and then return the stopCondition for endOfRun; important for  a couple of outside users"

	lastIndex := stopIndex.
	^#endOfRun