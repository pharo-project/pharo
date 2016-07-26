private
materialize: string
	^ STON reader
		on: (STONCStyleCommentsSkipStream on: string readStream);
		optimizeForLargeStructures;
		next