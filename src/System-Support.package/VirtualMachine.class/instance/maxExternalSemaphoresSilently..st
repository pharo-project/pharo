attributes
maxExternalSemaphoresSilently: aSize 
	"Changes the size of array where external signals for semaphores in
	externalObjects are handled.. 
	The size can only grow, and will always be the next power of two
	larger than the parameter.
	
	The intended use is to set the table size to some adequate
	maximum as part of a non-resuming image startUp.
	
	Setting this at any time other than start-up can potentially result in
	lost signals during reallocation.
	i.e. Requests handled during copying og signals from old to new array
	won't be seen if they occur to indices already copied, before
	pointers to the new and old arrays are switched."
	self maxExternalSemaphores
		ifNil: [^ 0].
	"The vm-header field where the size is stored is a short, maximum
	64k entries. 
	Well, on most platforms anyways"
	(aSize < 0
			or: [aSize > 65535])
		ifTrue: [^ DomainError signalFrom: 0 to: 65535].
	^ self parameterAt: 49 put: aSize