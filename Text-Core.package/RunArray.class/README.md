My instances provide space-efficient storage of data which tends to be constant over long runs of the possible indices. Essentially repeated values are stored singly and then associated with a "run" length that denotes the number of consecutive occurrences of the value.

My two important variables are
	runs	An array of how many elements are in each run
	values	An array of what the value is over those elements

The variables lastIndex, lastRun and lastOffset cache the last access
so that streaming through RunArrays is not an N-squared process.

Many complexities of access can be bypassed by using the method
	RunArray withStartStopAndValueDo: