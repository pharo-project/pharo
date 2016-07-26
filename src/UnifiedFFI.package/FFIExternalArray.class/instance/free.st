finalization
free
	handle ifNil: [ ^ self ]. 
	handle isExternalAddress 
		ifTrue: [ handle free ]
		ifFalse: [ handle := nil ]