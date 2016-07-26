accessing
stop
	^ sourcePointer 
		ifNil: [ -1 ]
		ifNotNil: [ self start + self length - 1 ]