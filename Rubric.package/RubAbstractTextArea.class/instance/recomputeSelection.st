accessing selection
recomputeSelection
	"The same characters are selected but their coordinates may have changed.
	Redetermine the selection according to the start and stop block indices; 
	do not highlight."

	self markIndex: self markIndex pointIndex: self pointIndex