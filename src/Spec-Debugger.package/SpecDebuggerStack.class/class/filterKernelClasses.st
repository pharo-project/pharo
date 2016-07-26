settings filters
filterKernelClasses
	^ KCFilterEnabled ifNil:[ KCFilterEnabled := false ]