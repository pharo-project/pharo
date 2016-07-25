instance creation
newOn: stack
	^ (self startAt: stack position)
		numMethodTempVars: stack numMethodTempVars;
		yourself