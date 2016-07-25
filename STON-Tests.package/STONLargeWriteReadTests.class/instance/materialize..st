private
materialize: string
	^ STON reader
		on: string readStream;
		optimizeForLargeStructures;
		next