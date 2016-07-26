computing
stepToNextScanLineAt: yValue in: edgeTableEntry
	"Compute the next x value for the scan line at yValue.
	This message is sent during incremental updates. 
	The yValue parameter is passed in here for edges
	that have more complicated computations,"
	^self intStepToNextScanLineAt: yValue in: edgeTableEntry