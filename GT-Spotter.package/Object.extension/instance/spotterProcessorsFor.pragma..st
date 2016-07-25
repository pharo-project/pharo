*GT-Spotter
spotterProcessorsFor: aSpotterStep pragma: aPragma
	" if there is a problem with the definition of one or more processors, let the other processors continue to work "
	 [ | processorsSize addedProcessorsSize |
		"To allow nice arrangement of processor within UI we need to assign to
		each processor its spotter order. Because it is possible
		to define multiple (or none) processors using pragma
		we need to detect how many new processors were added"
		processorsSize := aSpotterStep processors size.
		self perform: aPragma selector with: aSpotterStep.
		addedProcessorsSize := aSpotterStep processors size - processorsSize.
		"assign spotter order for newly added ones"
		(aSpotterStep processors last: addedProcessorsSize) do: [ :processor |
			processor order: (aPragma argumentAt: 1) ]
	]
		on: Error
		do: [ :exception | 
			aSpotterStep exceptionHandler
				handleException: exception
				do: [ 	('[Spotter] Exception while collecting processors for <', self class name, '>: ', exception asString) logCr ] ]